use std::str::FromStr;

use anyhow::{anyhow, Result};
use geo_types::Coord;
use image::{Rgba, RgbaImage};
use once_cell::sync::Lazy;
use rusqlite::{params, ToSql};
use serde::{Deserialize, Deserializer};

use crate::db::{decode_line, ActivityFilter, Database};
use crate::tile::{Tile, TileBounds};

pub static PINKISH: Lazy<LinearGradient> = Lazy::new(|| {
    LinearGradient::from_stops(&[
        (0.0, [0xff, 0xb1, 0xff, 0x7f]),
        (0.05, [0xff, 0xb1, 0xff, 0xff]),
        (0.25, [0xff, 0xff, 0xff, 0xff]),
    ])
});

pub static BLUE_RED: Lazy<LinearGradient> = Lazy::new(|| {
    LinearGradient::from_stops(&[
        (0.0, [0x3f, 0x5e, 0xfb, 0xff]),
        (0.05, [0xfc, 0x46, 0x6b, 0xff]),
        (0.25, [0xff, 0xff, 0xff, 0xff]),
    ])
});

pub static RED: Lazy<LinearGradient> = Lazy::new(|| {
    LinearGradient::from_stops(&[
        (0.0, [0xb2, 0x0a, 0x2c, 0xff]),
        (0.05, [0xff, 0xfb, 0xd5, 0xff]),
        (0.25, [0xff, 0xff, 0xff, 0xff]),
    ])
});

pub static ORANGE: Lazy<LinearGradient> = Lazy::new(|| {
    LinearGradient::from_stops(&[
        (0.0, [0xfc, 0x4a, 0x1a, 0xff]),
        (0.25, [0xf7, 0xb7, 0x33, 0xff]),
    ])
});

#[derive(Debug)]
pub struct LinearGradient {
    empty_value: Rgba<u8>,
    palette: [Rgba<u8>; 256],
}

struct TileRaster {
    bounds: TileBounds,
    scale: u32,
    width: u32,
    tile_extent: u32,
    pixels: Vec<u8>,
}

impl TileRaster {
    fn new(tile: Tile, source: TileBounds, width: u32, tile_extent: u32) -> Self {
        // TODO: support upscaling
        assert!(width <= tile_extent, "Upscaling not supported");
        assert!(width.is_power_of_two(), "width must be power of two");
        assert!(source.z >= tile.z, "source zoom must be >= target zoom");

        let zoom_steps = (source.z - tile.z) as u32;
        let width_steps = tile_extent.ilog2() - width.ilog2();

        Self {
            width,
            tile_extent,
            pixels: vec![0; (width * width) as usize],
            bounds: source,
            scale: zoom_steps + width_steps,
        }
    }

    fn add_activity(&mut self, source_tile: &Tile, coords: &[Coord<u32>]) {
        debug_assert_eq!(source_tile.z, self.bounds.z);

        // Origin of source tile within target tile
        let x_offset = self.tile_extent * (source_tile.x - self.bounds.xmin);
        let y_offset = self.tile_extent * (source_tile.y - self.bounds.ymin);

        let mut prev = None;
        for Coord { x, y } in coords {
            // Translate (x,y) to location in target tile.
            // [0..(width * STORED_TILE_WIDTH)]
            let x = x + x_offset;
            let y = (self.tile_extent - y) + y_offset;

            // Scale the coordinates back down to [0..width]
            let x = x >> self.scale;
            let y = y >> self.scale;

            if let Some(Coord { x: px, y: py }) = prev {
                if x == px && y == py {
                    continue;
                }

                let line_iter = line_drawing::Bresenham::<i32>::new(
                    (px as i32, py as i32),
                    (x as i32, y as i32),
                );

                for (ix, iy) in line_iter {
                    if ix < 0 || iy < 0 || ix >= self.width as i32 || iy >= self.width as i32 {
                        continue;
                    }

                    let idx = (iy as u32 * self.width + ix as u32) as usize;
                    self.pixels[idx] = self.pixels[idx].saturating_add(1);
                }
            }
            prev = Some(Coord { x, y });
        }
    }

    fn apply_gradient(&self, gradient: &LinearGradient) -> RgbaImage {
        RgbaImage::from_fn(self.width, self.width, |x, y| {
            let idx = (y * self.width + x) as usize;
            gradient.sample(self.pixels[idx])
        })
    }
}

/// Linearly interpolate between two colors
fn lerp(a: Rgba<u8>, b: Rgba<u8>, t: f32) -> Rgba<u8> {
    Rgba::from([
        (a[0] as f32 * (1.0 - t) + b[0] as f32 * t) as u8,
        (a[1] as f32 * (1.0 - t) + b[1] as f32 * t) as u8,
        (a[2] as f32 * (1.0 - t) + b[2] as f32 * t) as u8,
        0xff,
    ])
}

impl LinearGradient {
    pub fn from_stops<P>(stops: &[(f32, P)]) -> Self
    where
        P: Copy + Into<Rgba<u8>>,
    {
        let mut palette = [Rgba::from([0, 0, 0, 0]); 256];
        let mut i = 0;

        for stop in stops.windows(2) {
            let (a, b) = (&stop[0], &stop[1]);
            let width = (b.0 - a.0) * 256.0;

            let start_idx = (a.0 * 256.0).round() as usize;
            let end_idx = (b.0 * 256.0).ceil() as usize;

            while i < end_idx {
                let t = (i - start_idx) as f32 / width;
                palette[i] = lerp(a.1.into(), b.1.into(), t);

                i += 1;
            }
        }

        // Copy the last color to the end of the palette
        if let Some((_, last)) = stops.last() {
            while i < palette.len() {
                palette[i] = (*last).into();
                i += 1;
            }
        }

        LinearGradient {
            palette,
            empty_value: Rgba::from([0, 0, 0, 0]),
        }
    }

    pub fn sample(&self, val: u8) -> Rgba<u8> {
        if val == 0 {
            return self.empty_value;
        }

        self.palette[val as usize]
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct LinearGradientParseError;

impl FromStr for LinearGradient {
    type Err = LinearGradientParseError;

    /// Parse a string containing a list of stop points and colors, separated by a `;`.
    ///
    /// Colors may be written as `RGB`, `RRGGBB`, or `RRGGBBAA`
    ///
    /// For example: `0:001122;0.25:789;0.5:334455;0.75:ffffff33`
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let stops: Vec<(f32, Rgba<u8>)> = s
            .split(';')
            .map(|part| {
                let (pos, color) = part.split_once(':').ok_or(LinearGradientParseError)?;
                let pos = pos.parse::<f32>().map_err(|_| LinearGradientParseError)?;
                let color = {
                    let rgba = match color.len() {
                        3 => {
                            let rgb: String = color.chars().flat_map(|ch| [ch, ch]).collect();
                            format!("{}FF", rgb)
                        }
                        6 => format!("{color}FF"),
                        8 => color.to_string(),
                        _ => return Err(LinearGradientParseError),
                    };

                    u32::from_str_radix(&rgba, 16).map_err(|_| LinearGradientParseError)?
                };

                Ok((pos, Rgba::from(color.to_be_bytes())))
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(LinearGradient::from_stops(&stops))
    }
}

impl<'de> Deserialize<'de> for LinearGradient {
    fn deserialize<D>(deserializer: D) -> Result<LinearGradient, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        LinearGradient::from_str(&s).map_err(|_| serde::de::Error::custom("invalid gradient"))
    }
}

pub fn render_tile(
    tile: Tile,
    gradient: &LinearGradient,
    width: u32,
    filter: &ActivityFilter,
    db: &Database,
) -> Result<Option<RgbaImage>> {
    let zoom_level = db
        .config
        .source_level(tile.z)
        .ok_or_else(|| anyhow!("no source level for tile: {:?}", tile))?;

    let bounds = TileBounds::from(zoom_level, &tile);
    let mut raster = TileRaster::new(tile, bounds, width, db.config.tile_extent);

    let mut have_activity = false;

    let conn = db.ro_connection()?;
    let (mut stmt, params) = prepare_query_activities(&conn, filter, &bounds)?;
    let mut rows = stmt.query(params.as_slice())?;
    while let Some(row) = rows.next()? {
        let source_tile = Tile::new(row.get_unwrap(0), row.get_unwrap(1), row.get_unwrap(2));

        let bytes: Vec<u8> = row.get_unwrap(3);
        raster.add_activity(&source_tile, &decode_line(&bytes)?);

        have_activity = true;
    }

    if !have_activity {
        return Ok(None);
    }

    Ok(Some(raster.apply_gradient(gradient)))
}

fn prepare_query_activities<'a>(
    conn: &'a rusqlite::Connection,
    filter: &'a ActivityFilter,
    bounds: &'a TileBounds,
) -> Result<(rusqlite::Statement<'a>, Vec<&'a dyn ToSql>)> {
    let mut params = params![bounds.z, bounds.xmin, bounds.xmax, bounds.ymin, bounds.ymax].to_vec();
    let filter_clause = filter.to_query(&mut params);

    let stmt = conn.prepare(&format!(
        "\
        SELECT x, y, z, coords \
        FROM activity_tiles \
        JOIN activities ON activities.id = activity_tiles.activity_id \
        WHERE z = ? \
            AND (x >= ? AND x < ?) \
            AND (y >= ? AND y < ?) \
            AND {};",
        filter_clause,
    ))?;

    Ok((stmt, params))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_linear_gradient_parse() {
        let gradient = "0:001122;0.25:789;0.5:334455;0.75:ffffff33"
            .parse::<LinearGradient>()
            .unwrap();
        assert_eq!(gradient.palette[0], Rgba::from([0x00, 0x11, 0x22, 0xff]));
        assert_eq!(gradient.palette[64], Rgba::from([0x77, 0x88, 0x99, 0xff]));
        assert_eq!(gradient.palette[128], Rgba::from([0x33, 0x44, 0x55, 0xff]));
        assert_eq!(gradient.palette[255], Rgba::from([0xff, 0xff, 0xff, 0x33]));
    }
}
