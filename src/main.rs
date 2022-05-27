use std::collections::HashMap;
use std::path::Path;
use std::time::Instant;

use osmpbfreader::{objects::NodeId as OsmNodeId, OsmObj, OsmPbfReader, Tags};
use petgraph::{
    algo::astar,
    graph::NodeIndex,
    graph::{EdgeReference, Graph},
    Undirected,
};
use rand::Rng;

#[derive(Debug, Copy, Clone)]
struct Coordinate {
    lat: f32,
    lon: f32,
}

#[derive(Debug)]
struct NodeData {
    coord: Coordinate,
    // TODO: Extract only tags we care about
    tags: Option<Tags>,
}

#[derive(Debug, Clone)]
struct EdgeData {
    // meters
    //
    // TODO: Can likely store this as dist / 10 and fit in u16 (max: 655km)
    dist: u32,
    tags: Option<Tags>,
    // TODO: Can delta-encode coordinates against start point to fit in (u16, u16)
    // TODO: Alternatively - polyline, without ASCII representation
    geometry: Vec<Coordinate>,
}

struct OsmGraph {
    // TODO: use Csr, but petgraph doesn't support parallel edges, which we need.
    inner: Graph<NodeData, EdgeData, Undirected>,
}

impl Coordinate {
    // Haversine, returns meters
    // TODO: unchecked
    fn dist_to(&self, other: &Coordinate) -> u32 {
        let (lat1, lat2) = (self.lat.to_radians(), other.lat.to_radians());

        let dt_lon = (self.lon - other.lon).to_radians();
        let dt_lat = lat2 - lat1;

        let a = (dt_lat / 2.0_f32).sin();
        let b = (dt_lon / 2.0_f32).sin();
        let c = lat1.cos() * lat2.cos();
        let d = (a * a) + ((b * b) * c);
        let e = d.sqrt().asin();
        return (2_f32 * 6_372_800_f32 * e) as u32;
    }
}

fn is_way_routeable(tags: &Tags) -> bool {
    if tags.contains("route", "ferry") {
        return true;
    }

    if let Some(access) = tags.get("access") {
        match access.as_str() {
            "yes" | "permissive" | "delivery" | "designated" | "destination" | "agricultural"
            | "forestry" | "public" => (),
            &_ => return false,
        }
    }

    if let Some(highway) = tags.get("highway") {
        return match highway.as_str() {
            "bus_guideway" | "raceway" | "proposed" | "conveying" => false,
            &_ => true,
        };
    }

    return false;
}

enum NodeKind {
    Routing,
    Geometry,
}

fn construct_node_kind_map<R>(reader: &mut OsmPbfReader<R>) -> HashMap<OsmNodeId, NodeKind>
where
    R: std::io::Read + std::io::Seek,
{
    // A node is used for routing if it:
    // - is the first or last node of a way
    // - is shared by multiple ways (i.e. a junction)
    //
    // TODO: Could be a bitvector.
    let mut is_routing_node = HashMap::<OsmNodeId, bool>::new();

    for obj in reader.par_iter() {
        let obj = obj.unwrap();

        // TODO: Should we consider barrier nodes? Anything else that
        // we'd want to include if not in a way?
        if let OsmObj::Way(ref way) = obj {
            if way.nodes.len() < 2 || !is_way_routeable(&way.tags) {
                continue;
            }

            for (i, &osm_node_id) in way.nodes.iter().enumerate() {
                let should_retain = i == 0 || i == way.nodes.len() - 1;

                is_routing_node
                    .entry(osm_node_id)
                    .and_modify(|it| *it = true)
                    .or_insert(should_retain);
            }
        }
    }

    return is_routing_node
        .iter()
        .map(|(&osm_id, &is_routing)| {
            let kind = if is_routing {
                NodeKind::Routing
            } else {
                NodeKind::Geometry
            };

            (osm_id, kind)
        })
        .collect();
}

fn strip_tags(tags: &Tags) -> Option<Tags> {
    let mut tags = tags.clone();

    // Not exhaustive
    let unused_tags = vec![
        "created_by",
        "source",
        "ref",
        "addr:housenumber",
        "addr:street",
        "addr:city",
        "addr:postcode",
        "fixme",
        "comment",
        "note",
    ];

    for key in unused_tags.into_iter() {
        tags.remove(key);
    }

    if tags.is_empty() {
        return None;
    }

    Some(tags)
}

enum Pass2Node {
    Routing(NodeIndex<u32>, Coordinate),
    Geometry(Coordinate),
}

// TODO: we can avoid hashmaps and the ID assignment counters by using a bitmap
// TODO: Attach metadata to each edge
fn construct_graph(path: &Path) -> Result<OsmGraph, std::io::Error> {
    let f = std::fs::File::open(path).unwrap();
    let mut pbf = OsmPbfReader::new(f);

    let node_kind = construct_node_kind_map(&mut pbf);

    // Reset to start of file for next iteration.
    pbf.rewind().unwrap();

    let mut node_map = HashMap::<OsmNodeId, Pass2Node>::with_capacity(node_kind.len());

    let mut graph = Graph::<NodeData, EdgeData, _>::new_undirected();

    let mut seen_way = false;
    for obj in pbf.par_iter() {
        let obj = obj.unwrap();
        match obj {
            OsmObj::Relation(_) => {}

            OsmObj::Node(ref osm_node) => {
                assert!(seen_way == false, "input files MUST be sorted!");

                if let Some(node) = node_kind.get(&osm_node.id) {
                    let coord = Coordinate {
                        lat: osm_node.lat() as f32,
                        lon: osm_node.lon() as f32,
                    };

                    let n = match node {
                        NodeKind::Geometry => Pass2Node::Geometry(coord),
                        NodeKind::Routing => {
                            let id = graph.add_node(NodeData {
                                coord,
                                tags: strip_tags(&osm_node.tags),
                            });

                            Pass2Node::Routing(id, coord)
                        }
                    };

                    node_map.insert(osm_node.id, n);
                }
            }

            OsmObj::Way(ref way) => {
                seen_way = true;
                if way.nodes.len() < 2 || !is_way_routeable(&way.tags) {
                    continue;
                }

                let mut accumulated_dist = 0_u32;
                let mut prev_coord: Option<Coordinate> = None;
                let mut prev_node_id: Option<NodeIndex> = None;

                for osm_node_id in way.nodes.iter() {
                    let node = node_map
                        .get(osm_node_id)
                        .expect("Missing node info from way");

                    let coord = match node {
                        Pass2Node::Routing(_, coord) => coord,
                        Pass2Node::Geometry(coord) => coord,
                    };

                    if let Some(prev_coord) = prev_coord {
                        accumulated_dist += prev_coord.dist_to(coord);
                    }
                    prev_coord = Some(*coord);

                    if let Pass2Node::Routing(node_id, _) = node {
                        if let Some(prev_id) = prev_node_id {
                            // Duplicate edges are possible!
                            // - https://www.openstreetmap.org/way/1060404609
                            // - https://www.openstreetmap.org/way/1060404608

                            graph.add_edge(
                                *node_id,
                                prev_id,
                                EdgeData {
                                    dist: accumulated_dist,
                                    tags: strip_tags(&way.tags),
                                    // TODO: populate
                                    geometry: vec![],
                                },
                            );

                            accumulated_dist = 0;
                        }

                        prev_node_id = Some(*node_id);
                    }
                }
            }
        }
    }
    println!(
        "Created graph: nodes={}, edges={}",
        graph.node_count(),
        graph.edge_count()
    );

    Ok(OsmGraph { inner: graph })
}

// TODO: Seems that this can get the A-star impl in petgraph stuck if
// we use u32::MAX. (maybe an overflow?)
//
// Need some way of signaling "no, NEVER take this edge, I know
// there's an edge but I lied."
const INACCESSIBLE: u32 = 5_000_000;

impl OsmGraph {
    fn score_edge(&self, edge: EdgeReference<'_, EdgeData>) -> u32 {
        let edge_data = edge.weight();
        let tags = edge_data.tags.clone().unwrap();

        let mut multiple = if let Some(highway) = tags.get("highway") {
            match highway.as_str() {
                "motorway" | "motorway_link" => INACCESSIBLE,
                "trunk" | "trunk_link" => 10,
                "primary" | "primary_link" => 5,
                "path" | "steps" | "track" => 0,
                _ => 1,
            }
        } else {
            return INACCESSIBLE;
        };

        let max_multiple = 1000;
        if multiple > max_multiple {
            return INACCESSIBLE;
        }

        edge_data.dist * (1 + multiple)
    }

    // TODO: Build lat,lng -> NodeIndex lookup so we don't need to pass node index values.
    fn find_route(&self, from: NodeIndex, to: NodeIndex) -> Option<Vec<Coordinate>> {
        let dest_node = self.inner.node_weight(to).expect("Invalid `to` given.");

        let path = astar(
            &self.inner,
            from,
            |node_id| node_id == to,
            |e| self.score_edge(e),
            |node_id| {
                self.inner
                    .node_weight(node_id)
                    .map(|n| n.coord.dist_to(&dest_node.coord))
                    .unwrap_or(0)
            },
        );

        path.map(|(cost, path)| {
            println!("Total Cost = {:?} km (equiv)", cost as f32 / 1000.0);

            path.iter()
                .map(|node_id| {
                    // TODO: How do we figure out which edge was taken in the case of parallel ones?
                    self.inner
                        .node_weight(*node_id)
                        .expect("invalid from_node_id")
                        .coord
                })
                .collect()
        })
    }
}

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
            let geom = path
                .iter()
                .map(|coord| [coord.lon, coord.lat])
                .collect::<Vec<_>>();

            println!(" {{ \"type\": \"Feature\", \"geometry\": {{\"type\": \"LineString\", \"coordinates\": {:?}}}, \"properties\": {{}}}}", geom);
        }
    }

    timer.elapsed("complete");

    Ok(())
}
