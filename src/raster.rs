use std::f32::consts::PI;
use std::fs::File;
use std::io::{Result, Write};
use std::path::{Path, PathBuf};

use image::imageops;

use crate::geo::{MercatorPoint, EARTH_CIRCUM};
use crate::Point;

const TILE_SIZE: usize = 256;
const ORIGIN_OFFSET: f32 = EARTH_CIRCUM / 2.0;

#[derive(Copy, Clone, PartialEq, Debug)]
struct XYZTile {
    z: usize,
    x: usize,
    y: usize,
}

impl XYZTile {
    fn from_point(point: Point, z: usize) -> Self {
        let pow = (2 << (z - 1)) as f32;
        let lat_rad = point.lat.to_radians();

        Self {
            x: (pow * ((point.lng + 180.0) / 360.0)) as usize,
            y: (pow * (1.0 - (lat_rad.tan()).asinh() / PI) / 2.0) as usize,
            z,
        }
    }

    fn ul_xy(&self) -> MercatorPoint {
        let tile_size = EARTH_CIRCUM / (2 << (self.z - 1)) as f32;
        let (x, y) = (self.x as f32, self.y as f32);

        MercatorPoint {
            x: (x * tile_size - ORIGIN_OFFSET),
            y: (-y * tile_size + ORIGIN_OFFSET),
        }
    }

    fn br_xy(&self) -> MercatorPoint {
        let tile = XYZTile {
            x: self.x + 1,
            y: self.y + 1,
            z: self.z,
        };

        tile.ul_xy()
    }

    /// Return the pixel coordinates of `pt` on this tile (or None if out of bounds)
    fn project(&self, pt: Point) -> Option<(usize, usize)> {
        let point = pt.to_mercator()?;

        // TODO: clean this up
        let ul = self.ul_xy();
        let br = self.br_xy();

        let (width, height) = (br.x - ul.x, ul.y - br.y);
        let (x, y) = ((point.x - ul.x) / width, (ul.y - point.y) / height);

        let (x, y) = (
            (x * TILE_SIZE as f32) as usize,
            (y * TILE_SIZE as f32) as usize,
        );

        let valid_range = 0..TILE_SIZE;
        if valid_range.contains(&x) && valid_range.contains(&y) {
            Some((x, y))
        } else {
            None
        }
    }
}

// TODO: better name
pub struct MappedTile(Vec<u8>);

impl MappedTile {
    fn at(&self, x: usize, y: usize) -> u8 {
        self.0[y * TILE_SIZE + x]
    }
}

// TODO: Garmin heatmaps might be interesting as well. Quite simple,
// and map-matched to OSM;
//
// https://connecttile.garmin.com/{sport}/{z}/{x}/{y}.png
// sport: MOUTAIN_BIKING, ROAD_CYCLING, GRAVEL_BIKING, RUNNING, TRAIL_RUNNING
#[allow(unused)]
pub mod mapper {
    use image::Pixel;

    // TODO: untested
    #[inline]
    pub fn strava_mobile_blue(pxl: image::Rgba<u8>) -> u8 {
        let pxl = pxl.to_rgb();
        let rgb = pxl.channels();

        let max = rgb.iter().max().cloned().unwrap_or(0) as f32;
        let min = rgb.iter().min().cloned().unwrap_or(0) as f32;

        // Lightness
        let pct = (max + min) / 2.0;

        pct as u8
    }

    // Encodes heat into the green and alpha channels.
    //
    // green: 90 to 99 (increasing popularity)
    // alpha: 0 to 255 (increasing popularity)
    //
    // Assuming that green channel is logarithmic rather than linear,
    // but not sure
    #[inline]
    pub fn strava_orange(pxl: image::Rgba<u8>) -> u8 {
        let green = (pxl[1] as f32 - 90.0 + 1.0).log10();
        let alpha = pxl[3] as f32 / 255.0;

        (255.0 * green * alpha) as u8
    }
}

pub struct XYZTileSampler {
    tile_url: String,
    tile_dir: PathBuf,
    fixed_zoom: usize,
}

impl XYZTileSampler {
    pub fn new(tile_url: &str, tile_dir: &Path) -> Self {
        Self {
            tile_url: tile_url.into(),
            tile_dir: tile_dir.into(),
            fixed_zoom: 11,
        }
    }
    // TODO: use correct Result type here
    fn fetch_tile<F>(&self, xyz: XYZTile, path: &Path, pixel_mapper: F) -> Result<()>
    where
        F: Fn(image::Rgba<u8>) -> u8,
    {
        let url = self
            .tile_url
            .replace("{x}", &xyz.x.to_string())
            .replace("{y}", &xyz.y.to_string())
            .replace("{z}", &xyz.z.to_string());

        println!("[info] fetching {} ({:?})...", url, xyz);

        std::fs::create_dir_all(path.parent().unwrap())?;
        let mut file = File::create(path)?;

        let res = minreq::get(url).send().unwrap();

        match res.status_code {
            200 => {
                let mut img = image::load_from_memory(res.as_bytes())
                    .map(|i| i.into_rgba8())
                    .unwrap();

                let size = TILE_SIZE as u32;

                // Ensure we're working with identically sized images
                if img.width() != size || img.height() != size {
                    img = imageops::resize(&img, size, size, imageops::FilterType::Nearest);
                }

                let pixels: Vec<u8> = img.pixels().map(|&px| (pixel_mapper)(px)).collect();
                file.write_all(&pixels)?;

                Ok(())
            }

            // TODO: 404 could also indicate incorrectly configured
            // URL. How to distinguish?
            204 | 404 => {
                file.write_all(&[0; TILE_SIZE * TILE_SIZE])?;
                Ok(())
            }

            code => {
                // TODO: return as error
                panic!("Got error response: {}", code);
            }
        }
    }

    fn load_tile<F>(&self, xyz: XYZTile, pixel_mapper: F) -> Result<MappedTile>
    where
        F: Fn(image::Rgba<u8>) -> u8,
    {
        let tile_path = self
            .tile_dir
            .join(format!("{}/{}/{}.tile", xyz.z, xyz.x, xyz.y));

        if !tile_path.exists() {
            self.fetch_tile(xyz, &tile_path, pixel_mapper)?;
        }

        let bytes = std::fs::read(tile_path)?;
        Ok(MappedTile(bytes))
    }

    pub fn sample<F>(&self, points: &[Point], pixel_mapper: F) -> Result<f32>
    where
        F: Fn(image::Rgba<u8>) -> u8,
    {
        let mut sum = 0;
        let mut prev = None;

        for &pt in points {
            let tile = XYZTile::from_point(pt, self.fixed_zoom);

            if let Some((x, y)) = tile.project(pt) {
                let pixels = match prev {
                    Some((prev_tile, image)) if tile == prev_tile => image,
                    _ => self.load_tile(tile, &pixel_mapper)?,
                };

                sum += pixels.at(x, y) as u32;
                prev = Some((tile, pixels));
            }
        }

        Ok((sum as f32) / 255.0 / (points.len() as f32))
    }
}
