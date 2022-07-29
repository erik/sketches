#![allow(dead_code, unused_variables)]

use std::f32::consts::PI;
use std::path::PathBuf;

use crate::Point;

trait RasterSampler {
    fn sample(&self, points: &[Point]) -> Vec<f32>;
}

struct XYZTile {
    z: usize,
    x: usize,
    y: usize,
}

impl XYZTile {
    fn from(point: Point, z: usize) -> Self {
        let pow = (2 << z) as f32;
        let lat_rad = point.lat.to_radians();

        Self {
            x: (pow * ((point.lng + 180.0) / 360.0)) as usize,
            y: (pow * (1.0 - (lat_rad.tan()).asinh() / PI) / 2.0) as usize,
            z,
        }
    }

    /// Project [pt] to this tile
    fn pixel_offset(&self, pt: Point) -> Option<(usize, usize)> {
        let tile_size = 256;

        let size = (tile_size * (2 << self.z)) as f32 / (PI * 2.0);

        let x = (size * (PI + pt.lng)) as usize;
        let y = (size * (PI - (PI / 4.0 + pt.lat / 2.0).tan().ln())) as usize;

        if (0..tile_size).contains(&x) || (0..tile_size).contains(&y) {
            Some((x, y))
        } else {
            None
        }
    }
}

struct StravaHeatSampler {
    tile_dir: PathBuf,
    fixed_zoom: usize,
}

impl StravaHeatSampler {}

impl RasterSampler for StravaHeatSampler {
    fn sample(&self, points: &[Point]) -> Vec<f32> {
        todo!()
    }
}
