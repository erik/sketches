extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

mod geo;
mod graph;
mod index;
mod profile;
mod raster;
mod tags;

use std::path::Path;
use std::time::Instant;

use rocket::fairing::{Fairing, Info, Kind};
use rocket::fs::FileServer;
use rocket::http::Header;
use rocket::response::content::RawHtml;
use rocket::response::Responder;
use rocket::serde::json::Json;
use rocket::serde::{Deserialize, Serialize};
use rocket::{Request, Response, State};

use crate::geo::Point;
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

#[derive(Deserialize, Debug)]
#[serde(crate = "rocket::serde")]
pub struct RouteRequest {
    from: Point,
    to: Point,
}

#[derive(Serialize, Debug)]
#[serde(crate = "rocket::serde")]
pub struct RouteResponseBody {
    distance_meters: u32,
    route_cost: f32,
    geometry: Vec<Point>,
}

#[derive(Responder)]
pub enum BrouterResponse<R, E> {
    Err(E),

    #[response(content_type = "application/vnd.geo+json; charset=utf-8")]
    Ok(Json<R>),
}

#[derive(Serialize, Debug)]
#[serde(crate = "rocket::serde")]
pub struct BrouterProperties {
    creator: &'static str,
    #[serde(rename = "track-length")]
    track_length: String,
    cost: String,
    // Other, currently unsupported bits:
    //
    //   - filtered ascend [sic]
    //   - plain-ascend
    //   - total-time
    //   - total-energy
    //   - messages
    //   - times
}

#[derive(Serialize, Debug)]
#[serde(crate = "rocket::serde", tag = "type")]
pub struct FeatureCollection<P> {
    features: Vec<Feature<P>>,
}

#[derive(Serialize, Debug)]
#[serde(crate = "rocket::serde", tag = "type")]
pub struct Feature<P> {
    properties: P,
    geometry: Geometry,
}

#[derive(Serialize, Debug)]
#[serde(crate = "rocket::serde", tag = "type")]
pub enum Geometry {
    LineString { coordinates: Vec<(f32, f32, f32)> },
}

#[rocket::get("/")]
pub fn route_index() -> RawHtml<String> {
    let html = std::fs::read_to_string("web/index.html").unwrap();
    RawHtml(html)
}

#[rocket::post("/api/route", format = "json", data = "<req>")]
pub fn route_api(
    graph: &State<OsmGraph>,
    req: Json<RouteRequest>,
) -> Json<Option<RouteResponseBody>> {
    println!("Request: {:?}", req.0);

    let rt = load_runtime(&graph.tag_dict).expect("failed to load profile");

    let mut timer = Timer::new();
    let route = graph.find_route(&rt, req.0.from, req.0.to);
    timer.elapsed("find route");

    Json(route.map(|route| RouteResponseBody {
        distance_meters: route.dist_meters,
        route_cost: route.cost,
        geometry: route.geometry,
    }))
}

#[rocket::get("/api/brouter?<lonlats>&<format>")]
pub fn route_api_brouter(
    lonlats: &str,
    format: &str,
    graph: &State<OsmGraph>,
) -> BrouterResponse<FeatureCollection<BrouterProperties>, String> {
    assert_eq!(
        format, "geojson",
        "only `geojson` is supported for `format`"
    );

    let coords = lonlats
        .split('|')
        .map(|s| {
            let (lng, lat) = s.split_once(',')?;

            Some(Point {
                lng: lng.parse::<f32>().ok()?,
                lat: lat.parse::<f32>().ok()?,
            })
        })
        .into_iter()
        .collect::<Option<Vec<Point>>>();

    let (from, to) = match coords {
        Some(points) if points.len() == 2 => (points[0], points[1]),
        _ => panic!("improper format given for `lonlats`"),
    };

    let rt = load_runtime(&graph.tag_dict).expect("failed to load profile");

    let mut timer = Timer::new();
    let route = graph.find_route(&rt, from, to);
    timer.elapsed("find route");

    let route = match route {
        Some(route) => route,
        None => return BrouterResponse::Err("no valid route".into()),
    };

    BrouterResponse::Ok(Json(FeatureCollection {
        features: vec![Feature {
            properties: BrouterProperties {
                creator: "panamint",
                track_length: route.dist_meters.to_string(),
                cost: route.cost.to_string(),
            },
            geometry: Geometry::LineString {
                coordinates: route
                    .geometry
                    .iter()
                    .map(|pt| (pt.lng, pt.lat, 0.0))
                    .collect(),
            },
        }],
    }))
}

pub struct CORS;

#[rocket::async_trait]
impl Fairing for CORS {
    fn info(&self) -> Info {
        Info {
            name: "Attaching CORS headers to responses",
            kind: Kind::Response,
        }
    }

    async fn on_response<'r>(&self, _request: &'r Request<'_>, response: &mut Response<'r>) {
        response.set_header(Header::new("Access-Control-Allow-Origin", "*"));
        response.set_header(Header::new(
            "Access-Control-Allow-Methods",
            "POST, GET, PATCH, OPTIONS",
        ));
        response.set_header(Header::new("Access-Control-Allow-Headers", "*"));
        response.set_header(Header::new("Access-Control-Allow-Credentials", "true"));
    }
}

#[rocket::launch]
fn launch_server() -> _ {
    let graph = load_graph().expect("graph loading failed");

    let server = rocket::build()
        .manage(graph)
        .attach(CORS)
        .mount(
            "/",
            rocket::routes![route_index, route_api, route_api_brouter],
        )
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
