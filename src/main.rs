#![allow(dead_code)]

extern crate pest;
#[macro_use]
extern crate pest_derive;

mod graph;
mod index;
mod profile;
mod tags;

use std::path::Path;
use std::sync::{Arc, Mutex};
use std::time::Instant;

use rocket::serde::{Deserialize, Serialize};
use rocket::State;

use crate::graph::{construct_graph, OsmGraph};

struct Timer {
    started_at: Instant,
    last_marker_at: Instant,
}

impl Timer {
    fn new() -> Timer {
        let now = Instant::now();
        Timer {
            started_at: now,
            last_marker_at: now,
        }
    }

    fn elapsed(&mut self, msg: &str) {
        let now = Instant::now();
        let elapsed = now - self.last_marker_at;
        let total_elapsed = now - self.started_at;
        self.last_marker_at = now;

        println!(
            "====================== {:30} dt={:.8} ms, tot={:.8} ms",
            msg,
            elapsed.as_millis(),
            total_elapsed.as_millis()
        );
    }

    fn reset(&mut self) {
        self.last_marker_at = Instant::now();
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(crate = "rocket::serde")]
struct JsonLatLng {
    lat: f32,
    lon: f32,
}

impl Into<crate::graph::LatLng> for JsonLatLng {
    fn into(self) -> crate::graph::LatLng {
        crate::graph::LatLng {
            lat: self.lat.to_radians(),
            lon: self.lon.to_radians(),
        }
    }
}
impl Into<JsonLatLng> for crate::graph::LatLng {
    fn into(self) -> JsonLatLng {
        JsonLatLng {
            lat: self.lat.to_degrees(),
            lon: self.lon.to_degrees(),
        }
    }
}

#[derive(Deserialize, Debug)]
#[serde(crate = "rocket::serde")]
pub struct RouteRequest {
    from: JsonLatLng,
    to: JsonLatLng,
}

#[derive(Serialize, Debug)]
#[serde(crate = "rocket::serde")]
pub struct RouteResponse {
    route: Option<Vec<JsonLatLng>>,
}

mod routes {
    use rocket::response::content::RawHtml;
    use rocket::serde::json::Json;

    use super::*;

    #[rocket::get("/")]
    pub fn index() -> RawHtml<&'static [u8]> {
        RawHtml(include_bytes!("index.html"))
    }

    #[rocket::post("/route", format = "json", data = "<req>")]
    pub fn route(
        graph: &State<Arc<Mutex<OsmGraph>>>,
        req: Json<RouteRequest>,
    ) -> Json<RouteResponse> {
        println!("Incoming routing request: {:?}", req.0);
        let graph = graph.lock().unwrap();
        let route = graph.find_route(req.0.from.into(), req.0.to.into());

        Json(RouteResponse {
            route: route.map(|points| points.into_iter().map(|pt| pt.into()).collect()),
        })
    }
}

#[rocket::launch]
fn launch_server() -> _ {
    let graph = Arc::new(Mutex::new(load_graph().expect("graph loading failed")));

    rocket::build()
        .manage(graph)
        .mount("/", rocket::routes![routes::index, routes::route])
}

fn load_graph() -> Result<OsmGraph, std::io::Error> {
    let mut timer = Timer::new();

    // TODO: Real argument parsing
    let args: Vec<String> = std::env::args().collect();
    let osm_path = match args.get(1) {
        Some(path) => path.as_str(),
        None => "./data/andorra.osm.pbf",
    };

    // TODO: save/load graph so it doesn't need to be constructed
    // every time.
    let graph = construct_graph(Path::new(&osm_path))?;
    timer.elapsed("build graph");

    Ok(graph)
}
