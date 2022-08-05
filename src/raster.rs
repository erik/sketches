#![allow(dead_code, unused_variables)]

use std::f32::consts::PI;
use std::fs::File;
use std::io::Result;
use std::path::{Path, PathBuf};

use image::RgbaImage;

use crate::Point;

#[derive(Copy, Clone, PartialEq, Debug)]
struct XYZTile {
    z: usize,
    x: usize,
    y: usize,
}

impl XYZTile {
    fn from_point(point: Point, z: usize) -> Self {
        let pow = (2 << z) as f32;
        let lat_rad = point.lat.to_radians();

        Self {
            x: (pow * ((point.lng + 180.0) / 360.0)) as usize,
            y: (pow * (1.0 - (lat_rad.tan()).asinh() / PI) / 2.0) as usize,
            z,
        }
    }

    /// Project [pt] to this tile
    fn pixel_offset(&self, pt: Point) -> Option<(u32, u32)> {
        let tile_size = 256;

        let size = (tile_size * (2 << self.z)) as f32 / (PI * 2.0);

        let x = (size * (PI + pt.lng)) as u32;
        let y = (size * (PI - (PI / 4.0 + pt.lat / 2.0).tan().ln())) as u32;

        if (0..tile_size).contains(&x) || (0..tile_size).contains(&y) {
            Some((x, y))
        } else {
            None
        }
    }
}

pub mod mapper {
    use image::Pixel;

    // TODO: untested
    pub fn strava_mobile_blue(pxl: image::Rgba<u8>) -> f32 {
        let pxl = pxl.to_rgb();
        let rgb = pxl.channels();

        let max = rgb.iter().max().unwrap_or(&0);
        let min = rgb.iter().min().unwrap_or(&0);

        // Lightness
        (max + min) as f32 / 2.0
    }

    // TODO: untested
    pub fn strava_orange(pxl: image::Rgba<u8>) -> f32 {
        let pxl = pxl.to_rgba();
        let rgba = pxl.channels();

        rgba[3] as f32 / 256.0
    }
}

struct XYZTileSampler {
    tile_url: String,
    tile_dir: PathBuf,
    fixed_zoom: usize,
}

impl XYZTileSampler {
    // TODO: use correct Result type here
    fn fetch_tile(&self, xyz: XYZTile, path: &Path) -> Result<()> {
        let url = self
            .tile_url
            .replace("{x}", &xyz.x.to_string())
            .replace("{y}", &xyz.y.to_string())
            .replace("{z}", &xyz.z.to_string());

        println!("[info] fetching {}...", url);

        let mut res = reqwest::blocking::get(url).unwrap();
        let mut file = File::create(path)?;
        res.copy_to(&mut file).unwrap();

        Ok(())
    }

    fn load_tile(&self, xyz: XYZTile) -> Result<RgbaImage> {
        // TODO: not necessarily PNG
        let tile_path = self
            .tile_dir
            .join(format!("{}/{}/{}.png", xyz.z, xyz.x, xyz.y));

        if !tile_path.exists() {
            self.fetch_tile(xyz, &tile_path)?;
        }

        let image = image::open(tile_path).unwrap().into_rgba8();
        Ok(image)
    }

    fn sample<F>(&mut self, points: &[Point], pixel_mapper: F) -> Result<Vec<Option<f32>>>
    where
        F: Fn(image::Rgba<u8>) -> f32,
    {
        let mut values = Vec::with_capacity(points.len());
        let mut prev = None;

        for &pt in points {
            let tile = XYZTile::from_point(pt, self.fixed_zoom);
            let image = match prev {
                Some((prev_tile, image)) if tile == prev_tile => image,
                _ => self.load_tile(tile)?,
            };

            let pixel_coord = tile.pixel_offset(pt);
            let value = pixel_coord.map(|c| {
                let pixel = image[c];
                (pixel_mapper)(pixel)
            });

            values.push(value);

            prev = Some((tile, image));
        }

        Ok(values)
    }
}
