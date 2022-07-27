extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

mod geo;
mod graph;
mod index;
mod profile;
mod tags;

use std::path::Path;
use std::time::Instant;

use rocket::fs::FileServer;
use rocket::serde::{Deserialize, Serialize};
use rocket::State;

use crate::graph::OsmGraph;
use crate::profile::Runtime;
use crate::tags::TagDict;

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
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(crate = "rocket::serde")]
struct JsonLatLng {
    lat: f32,
    lon: f32,
}

impl From<crate::graph::LatLng> for JsonLatLng {
    fn from(ll: crate::graph::LatLng) -> JsonLatLng {
        JsonLatLng {
            lat: ll.lat.to_degrees(),
            lon: ll.lon.to_degrees(),
        }
    }
}
impl From<JsonLatLng> for crate::graph::LatLng {
    fn from(json: JsonLatLng) -> crate::graph::LatLng {
        crate::graph::LatLng {
            lat: json.lat.to_radians(),
            lon: json.lon.to_radians(),
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
pub struct RouteResponseBody {
    distance_meters: u32,
    route_cost: f32,
    geometry: Vec<JsonLatLng>,
}

mod routes {
    use rocket::response::content::RawHtml;
    use rocket::serde::json::Json;

    use super::*;

    #[rocket::get("/")]
    pub fn index() -> RawHtml<String> {
        let html = std::fs::read_to_string("web/index.html").unwrap();
        RawHtml(html)
    }

    #[rocket::post("/route", format = "json", data = "<req>")]
    pub fn route(
        graph: &State<OsmGraph>,
        req: Json<RouteRequest>,
    ) -> Json<Option<RouteResponseBody>> {
        println!("Request: {:?}", req.0);

        let rt = load_runtime(&graph.tag_dict).expect("failed to load profile");

        let mut timer = Timer::new();
        let route = graph.find_route(&rt, req.0.from.into(), req.0.to.into());
        timer.elapsed("find route");

        Json(route.map(|route| RouteResponseBody {
            distance_meters: route.dist_meters,
            route_cost: route.cost,
            geometry: route.geometry.into_iter().map(|pt| pt.into()).collect(),
        }))
    }
}

#[rocket::launch]
fn launch_server() -> _ {
    let graph = load_graph().expect("graph loading failed");

    let server = rocket::build()
        .manage(graph)
        .mount("/", rocket::routes![routes::index, routes::route])
        .mount("/static", FileServer::from("web/"));

    println!("Ready to go!");

    server
}

fn load_runtime(tag_dict: &TagDict) -> Result<Runtime, std::io::Error> {
    let source = std::fs::read_to_string("profiles/cxb.mint")?;
    Ok(Runtime::from_source(&source, tag_dict).unwrap())
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
    let graph = OsmGraph::from_osm(Path::new(&osm_path))?;
    timer.elapsed("build graph");

    Ok(graph)
}
