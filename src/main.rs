#![allow(dead_code)]

mod graph;
mod index;
mod tags;

use std::path::Path;
use std::time::Instant;

use petgraph::graph::NodeIndex;
use rand::Rng;

use crate::graph::construct_graph;

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

fn main() -> Result<(), std::io::Error> {
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

    let mut rng = rand::thread_rng();

    for _ in 0..3 {
        let node_range = 0..graph.inner.node_count();
        let from = NodeIndex::new(rng.gen_range(node_range.clone()));
        let to = NodeIndex::new(rng.gen_range(node_range));

        timer.reset();
        let route = graph.find_route(from, to);
        timer.elapsed("find route");

        if let Some(path) = route {
            let geom = path.iter().map(|pt| pt.to_degrees()).collect::<Vec<_>>();

            println!(" {{ \"type\": \"Feature\", \"geometry\": {{\"type\": \"LineString\", \"coordinates\": {:?}}}, \"properties\": {{}}}}", geom);
        }
    }

    timer.elapsed("complete");

    Ok(())
}
