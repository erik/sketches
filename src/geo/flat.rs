//! Flat-earth geometry. Based on cheap-ruler.

use super::{Point, EARTH_RADIUS};

/// Inverse flattening of earth sphere
const FE: f32 = 1.0 / 298.257_23;
const E2: f32 = FE * (2.0 - FE);

lazy_static! {
    static ref RULERS: Vec<Ruler> = (0..=90).map(|deg| Ruler::new(deg as f32)).collect();
}

#[derive(PartialEq, Debug)]
pub struct Ruler {
    /// Meters per degree of longitude
    k_lng: f32,
    /// Meters per degree of latitude
    k_lat: f32,
}

impl Ruler {
    pub fn for_lat(lat: f32) -> &'static Self {
        let lat = lat.abs() as usize;
        &RULERS[lat]
    }

    #[inline]
    pub fn for_point(p: &Point) -> &'static Self {
        Self::for_lat(p.lat)
    }

    pub fn new(lat: f32) -> Self {
        let m = EARTH_RADIUS.to_radians();
        let cos_lat = (lat.to_radians()).cos();
        let w2 = 1.0 / (1.0 - E2 * (1.0 - cos_lat * cos_lat));
        let w = w2.sqrt();

        Self {
            k_lng: m * w * cos_lat,
            k_lat: m * w * w2 * (1.0 - E2),
        }
    }

    /// (lng, lat) delta between two points, in meters.
    #[inline]
    fn dist_xy(&self, a: &Point, b: &Point) -> (f32, f32) {
        let d_lng = wrap_degree(b.lng - a.lng) * self.k_lng;
        let d_lat = (b.lat - a.lat) * self.k_lat;

        (d_lng, d_lat)
    }

    pub fn dist_cheap(&self, a: &Point, b: &Point) -> f32 {
        let (d_lng, d_lat) = self.dist_xy(a, b);

        d_lng.hypot(d_lat)
    }

    pub fn meters_to_deg(&self, m: f32) -> f32 {
        let deg_per_m = 1.0 / self.k_lng;
        m * deg_per_m
    }

    pub fn line_dist(&self, geo: &[Point]) -> f32 {
        geo.iter()
            .enumerate()
            .skip(1)
            .map(|(i, p)| self.dist_cheap(&geo[i - 1], p))
            .sum()
    }

    /// Return a new line created by interpolating points along the
    /// original line at equal intervals.
    pub fn resample_line(&self, geo: &[Point], meters: f32) -> Vec<Point> {
        assert!(geo.len() > 0);

        let mut resampled = vec![geo[0]];

        let total_length = self.line_dist(geo);
        let num_segments = (total_length / meters).ceil();
        let frac = 1.0 / num_segments;
        let mut ratio = 0.0;

        for _ in 1..(num_segments as usize) {
            ratio += frac;

            resampled.push(self.line_interpolate_point(geo, ratio));
        }

        resampled
    }

    fn line_interpolate_point(&self, geo: &[Point], fractional_length: f32) -> Point {
        let mut acc_length = 0.0;

        for (i, point) in geo.iter().enumerate().skip(1) {
            let prev = &geo[i - 1];
            let length = self.dist_cheap(point, prev);

            if acc_length + length >= fractional_length {
                let frac = (fractional_length - acc_length) / length;
                let (dx, dy) = self.dist_xy(point, prev);

                return Point {
                    lat: prev.lat + dy * frac,
                    lng: prev.lng + dx * frac,
                };
            }

            acc_length += length;
        }

        panic!("shouldn't be able to get here");
    }

    // TODO: use this for simplification
    /// Shortest distance from a point to a line
    #[allow(dead_code)]
    pub fn dist_point_to_line(&self, pt: &Point, line: &(Point, Point)) -> f32 {
        let (a, b) = line;
        let line_pt = {
            // Line length
            let (dx, dy) = self.dist_xy(a, b);
            // Dist from point to start of line
            let (dpx, dpy) = self.dist_xy(a, pt);

            let t = (dx * dpx + dy * dpy) / (dx * dx + dy * dy);
            if t > 1.0 {
                *b
            } else if t > 0.0 {
                Point {
                    lng: a.lng + (dx / self.k_lng) * t,
                    lat: a.lat + (dy / self.k_lat) * t,
                }
            } else {
                *a
            }
        };

        let (dx, dy) = self.dist_xy(pt, &line_pt);
        dx.hypot(dy)
    }
}

#[inline]
fn wrap_degree(mut deg: f32) -> f32 {
    while deg < -180.0 {
        deg += 360.0
    }

    while deg > 180.0 {
        deg -= 360.0
    }
    deg
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sanity_check() {
        let ruler = Ruler::new(0.0);

        let a = &Point {
            lng: 180.0,
            lat: 0.0,
        };
        let b = &Point { lng: 0.0, lat: 0.0 };

        let earth_circ = EARTH_RADIUS * 2.0 * std::f32::consts::PI;
        assert_eq!(ruler.dist_cheap(a, b), earth_circ / 2.0);
    }

    #[test]
    fn meters_to_degrees_at_equator() {
        let ruler = Ruler::new(0.0);

        let expected = 0.000_008_9;
        let actual = ruler.meters_to_deg(1.0);
        let delta = (actual - expected).abs();

        assert!(
            delta < 1e-7,
            "actual = {}, expected = {}, delta = {}",
            actual,
            expected,
            delta
        );
    }

    #[test]
    fn haversine_equivalence() {
        let base = &Point {
            lng: 96.920341,
            lat: 32.838261,
        };

        let points: Vec<Point> = [
            (96.920341, 32.838261),
            (96.920421, 32.838295),
            (96.920421, 32.838295),
            (96.920536, 32.838297),
            (96.920684, 32.838293),
            (96.920818, 32.838342),
        ]
        .into_iter()
        .map(|(lng, lat)| Point { lng, lat })
        .collect();

        let ruler = Ruler::new(32.8351);

        for pt in points.iter() {
            let haver_dist = base.haversine(pt);
            let cheap_dist = ruler.dist_cheap(base, pt);

            let delta = (haver_dist - cheap_dist).abs();

            println!("Haversine: {}\nFlat earth: {}", haver_dist, cheap_dist);

            // TODO: Half a meter delta is quite poor, we should be able to do better.
            assert!(delta < 0.5);
        }
    }

    #[test]
    fn get_for_lat() {
        let ruler1 = Ruler::new(90.0);
        let ruler2 = Ruler::for_lat(90.5);

        assert_eq!(ruler1, *ruler2);
    }

    #[test]
    fn dist_point_to_line() {
        let ruler = Ruler::new(32.8351);

        let pt = Point {
            lat: 38.882017,
            lng: -77.034076,
        };

        let line = (
            Point {
                lat: 38.878605,
                lng: -77.031669,
            },
            Point {
                lat: 38.881946,
                lng: -77.029609,
            },
        );

        let actual = ruler.dist_point_to_line(&pt, &line);
        let expected = 374.6;

        let delta = (actual - expected).abs();
        assert!(delta < 0.5, "expected: {}, actual: {}", expected, actual);
    }
}
