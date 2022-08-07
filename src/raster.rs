#![allow(dead_code, unused_variables)]

use std::f32::consts::PI;
use std::fs::File;
use std::io::{Result, Write};
use std::path::{Path, PathBuf};

use crate::Point;

const TILE_SIZE: usize = 512;

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

    /// Project [pt] to this tile
    fn pixel_offset(&self, pt: Point) -> Option<(usize, usize)> {
        let pow = 2 << (self.z - 1);
        let size = (TILE_SIZE * pow) as f32 / (PI * 2.0);

        let x = (size * (PI + pt.lng)) as usize;
        let y = (size * (PI - (PI / 4.0 + pt.lat / 2.0).tan().ln())) as usize;

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

pub mod mapper {
    use image::Pixel;

    // TODO: untested
    pub fn strava_mobile_blue(pxl: image::Rgba<u8>) -> u8 {
        let pxl = pxl.to_rgb();
        let rgb = pxl.channels();

        let max = rgb.iter().max().unwrap_or(&0);
        let min = rgb.iter().min().unwrap_or(&0);

        // Lightness
        let pct = (max + min) as f32 / 2.0;

        (pct * 255.0) as u8
    }

    // TODO: untested
    pub fn strava_orange(pxl: image::Rgba<u8>) -> u8 {
        let pxl = pxl.to_rgba();
        let rgba = pxl.channels();

        rgba[3]
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

        println!("[info] fetching {:?}...", xyz);

        std::fs::create_dir_all(path.parent().unwrap())?;
        let mut file = File::create(path)?;

        let res = minreq::get(url).send().unwrap();
        let img = image::load_from_memory(res.as_bytes())
            .map(|i| i.into_rgba8())
            .unwrap();

        let pixels: Vec<u8> = img.pixels().map(|&px| (pixel_mapper)(px)).collect();

        file.write_all(&pixels)?;

        Ok(())
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

    pub fn sample<F>(&self, points: &[Point], pixel_mapper: F) -> Result<Vec<u8>>
    where
        F: Fn(image::Rgba<u8>) -> u8,
    {
        let mut values = Vec::with_capacity(points.len());
        let mut prev = None;

        for &pt in points {
            let tile = XYZTile::from_point(pt, self.fixed_zoom);
            let pixels = match prev {
                Some((prev_tile, image)) if tile == prev_tile => image,
                _ => self.load_tile(tile, &pixel_mapper)?,
            };

            if let Some((x, y)) = tile.pixel_offset(pt) {
                let value = pixels.at(x, y);
                values.push(value);
            }

            prev = Some((tile, pixels));
        }

        Ok(values)
    }
}
