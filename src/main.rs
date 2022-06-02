#![allow(dead_code)]

mod graph;
mod index;
mod tags;

use std::path::Path;
use std::time::Instant;

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

#[rocket::get("/")]
fn route_index(graph: &State<OsmGraph>) -> String {
    format!("your graph has {:?} nodes\n", graph.inner.node_count())
}

#[rocket::launch]
fn launch_server() -> _ {
    let graph = load_graph().expect("graph loading failed");

    rocket::build()
        .manage(graph)
        .mount("/", rocket::routes![route_index])
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
