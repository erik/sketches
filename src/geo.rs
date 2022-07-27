// TODO: real serde
use rocket::serde::{Deserialize, Serialize};

pub mod flat;

/// Radius of the earth at equator, meters
const EARTH_RADIUS: f32 = 6_378_137.0;

#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq)]
#[serde(crate = "rocket::serde")]
pub struct Point {
    pub lat: f32,
    pub lng: f32,
}

impl Point {
    // Haversine distance, returns meters
    pub fn haversine(&self, other: &Self) -> f32 {
        let (lat1, lng1) = (self.lat.to_radians(), self.lng.to_radians());
        let (lat2, lng2) = (other.lat.to_radians(), other.lng.to_radians());

        let dt_lng = (lng1 - lng2).abs();
        let dt_lat = (lat1 - lat2).abs();

        let a = (dt_lat / 2.0).sin();
        let b = (dt_lng / 2.0).sin();
        let c = lat1.cos() * lat2.cos();
        let d = (a * a) + ((b * b) * c);
        let e = d.sqrt().asin();

        2.0 * EARTH_RADIUS * e
    }
}

// Ramer-Douglas-Peucker line simplification
// TODO: needs tests, not checked
pub fn simplify_line(geo: &[Point], epsilon: f32) -> Vec<Point> {
    let mut result = Vec::with_capacity(geo.len());
    result.push(geo[0]);
    simplify_inner(geo, epsilon, &mut result);
    result
}

// TODO: Probably shouldn't be working in radians here...
fn simplify_inner(geo: &[Point], epsilon: f32, result: &mut Vec<Point>) {
    if geo.len() < 2 {
        return;
    }

    let (first, last) = (geo[0], geo[geo.len() - 1]);

    let dy = last.lat - first.lat;
    let dx = last.lng - first.lng;

    let mut max_dist = 0.0;
    let mut index = 0;

    for i in 1..geo.len() - 1 {
        let p = geo[i];
        // Distance from `point` to line [first, last]
        let d = (p.lng * dy - p.lat * dx) + (last.lng * first.lat - last.lat * first.lng);
        let dist = d.abs() / dx.hypot(dy);

        if dist > max_dist {
            max_dist = dist;
            index = i;
        }
    }

    if max_dist > epsilon {
        simplify_inner(&geo[..=index], epsilon, result);
        simplify_inner(&geo[index..], epsilon, result);
    } else {
        result.push(last);
    }
}
