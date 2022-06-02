#![allow(dead_code)]

use std::collections::HashMap;
use std::convert::From;
use std::path::Path;
use std::time::Instant;

use osmpbfreader::{
    objects::{Node as OsmNode, NodeId as OsmNodeId},
    OsmObj, OsmPbfReader, Tags,
};
use petgraph::{
    algo::astar,
    graph::NodeIndex,
    graph::{EdgeReference, Graph},
    Undirected,
};
use rand::Rng;

mod index;
mod tags;

use crate::index::{IndexedCoordinate, Point2D, SpatialIndex};
use crate::tags::{EdgeTags, NodeTags};

/// In radians
#[derive(Debug, Copy, Clone, PartialEq)]
struct LatLng {
    lat: f32,
    lon: f32,
}

#[derive(Debug)]
struct NodeData {
    point: LatLng,
    tags: NodeTags,
}

#[derive(Debug, Clone)]
struct EdgeData {
    // meters
    // TODO: Can likely store this as dist / 10 and fit in u16 (max: 655km)
    dist: u32,
    tags: EdgeTags,
    // TODO: Can delta-encode coordinates against start point to fit in (u16, u16)
    // TODO: Alternatively - polyline, without ASCII representation
    // TODO: Simplify geometry before storing
    geometry: Vec<LatLng>,
}

struct OsmGraph {
    // TODO: use Csr, but petgraph doesn't support parallel edges, which we need.
    // TODO: Use a directed graph so we can represent one ways etc.
    inner: Graph<NodeData, EdgeData, Undirected>,
    index: SpatialIndex<NodeIndex, Point2D>,
}

impl LatLng {
    // Haversine, returns meters
    // TODO: unchecked
    fn dist_to(&self, other: &Self) -> f32 {
        let dt_lon = self.lon - other.lon;
        let dt_lat = self.lat - other.lat;

        let a = (dt_lat / 2.0_f32).sin();
        let b = (dt_lon / 2.0_f32).sin();
        let c = self.lat.cos() * other.lat.cos();
        let d = (a * a) + ((b * b) * c);
        let e = d.sqrt().asin();

        2_f32 * 6_372_800_f32 * e
    }

    // TODO: Clumsy typing for geojson
    fn to_degrees(&self) -> [f32; 2] {
        [self.lon.to_degrees(), self.lat.to_degrees()]
    }
}

impl From<&OsmNode> for LatLng {
    fn from(node: &OsmNode) -> LatLng {
        let (lat, lon) = (node.lat() as f32, node.lon() as f32);

        return LatLng {
            lat: lat.to_radians(),
            lon: lon.to_radians(),
        };
    }
}

impl From<LatLng> for (f32, f32) {
    fn from(pt: LatLng) -> (f32, f32) {
        (pt.lat, pt.lon)
    }
}

const WAY_PERMITTED_ACCESS_VALUES: &[&str] =
    &["yes", "permissive", "designated", "destination", "public"];
const WAY_UNROUTABLE_HIGHWAY_VALUES: &[&str] =
    &["bus_guideway", "raceway", "proposed", "construction"];

/// Return if the OSM Way has tags that are relevant to routability
///
/// Reference: https://wiki.openstreetmap.org/wiki/OSM_tags_for_routing
fn is_way_routeable(tags: &Tags) -> bool {
    if tags.contains("route", "ferry") {
        return true;
    }

    // TODO: What about something like access=no + bicycle=yes?
    match tags.get("access") {
        Some(val) if !WAY_PERMITTED_ACCESS_VALUES.contains(&val.as_str()) => return false,
        Some(_) => {}
        None => {}
    }

    match tags.get("highway") {
        Some(val) if WAY_UNROUTABLE_HIGHWAY_VALUES.contains(&val.as_str()) => return false,
        Some(_) => return true,
        None => {}
    };

    // If we don't have a highway=*, we need to at least have a junction
    tags.contains_key("junction")
}

const NODE_HIGHWAY_ROUTING_VALUES: &[&str] = &[
    "crossing",
    "mini_roundabout",
    "motorway_junction",
    "stop",
    "traffic_signals",
    "turning_circle",
    "turning_loop",
];

const NODE_ROUTING_TAGS: &[&str] = &["ford", "bicycle", "access", "barrier", "junction"];

fn is_node_used_for_routing(tags: &Tags) -> bool {
    match tags.get("highway") {
        Some(val) if NODE_HIGHWAY_ROUTING_VALUES.contains(&val.as_str()) => return true,
        Some(_) => {}
        None => {}
    }

    return NODE_ROUTING_TAGS.iter().any(|&t| tags.contains_key(t));
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
    let mut node_kind_mapping = HashMap::<OsmNodeId, NodeKind>::new();
    let mut keys = std::collections::HashSet::<_>::new();
    let mut vals = std::collections::HashSet::<_>::new();
    let mut node_tags = 0;
    let mut way_tags = 0;

    for obj in reader.par_iter() {
        let obj = obj.unwrap();

        match obj {
            OsmObj::Node(ref node) => {
                if is_node_used_for_routing(&node.tags) {
                    node_kind_mapping.insert(node.id, NodeKind::Routing);
                }

                for (k, v) in node.tags.iter() {
                    node_tags += 1;
                    keys.insert(k.clone());
                    vals.insert(v.clone());
                }
            }

            OsmObj::Way(ref way) => {
                for (k, v) in way.tags.iter() {
                    way_tags += 1;
                    keys.insert(k.clone());
                    vals.insert(v.clone());
                }
                if way.nodes.len() < 2 || !is_way_routeable(&way.tags) {
                    continue;
                }

                for (i, &osm_node_id) in way.nodes.iter().enumerate() {
                    let is_first_or_last = i == 0 || i == way.nodes.len() - 1;

                    node_kind_mapping
                        .entry(osm_node_id)
                        // If there's an existing entry, it's a either a
                        // junction or already a routing node.
                        .and_modify(|it| *it = NodeKind::Routing)
                        .or_insert(if is_first_or_last {
                            NodeKind::Routing
                        } else {
                            NodeKind::Geometry
                        });
                }
            }

            // TODO: Is this needed?
            OsmObj::Relation(_) => {}
        }
    }

    println!(
        "Data Size: nodes={:?} ways={:?} keys={:?}, vals={:?}",
        node_tags,
        way_tags,
        keys.len(),
        vals.len()
    );

    node_kind_mapping
}

fn strip_tags(tags: &Tags) -> Tags {
    let mut tags = tags.clone();

    // Not exhaustive
    let unused_tags = vec![
        "addr:city",
        "addr:housenumber",
        "addr:postcode",
        "addr:street",
        "comment",
        "created_by",
        "fixme",
        "note",
        "ref",
        "source",
        // TODO: remove
        "name",
    ];

    for key in unused_tags.into_iter() {
        tags.remove(key);
    }

    tags
}

enum Pass2Node {
    Routing(NodeIndex<u32>, LatLng),
    Geometry(LatLng),
}

// TODO: Extract out some kind of helper functions, too big.
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
                assert!(!seen_way, "input files MUST be sorted!");

                node_kind
                    .get(&osm_node.id)
                    .map(|kind| {
                        let point = LatLng::from(osm_node);
                        match kind {
                            NodeKind::Geometry => Pass2Node::Geometry(point),
                            NodeKind::Routing => {
                                let id = graph.add_node(NodeData {
                                    point,
                                    tags: NodeTags::from(strip_tags(&osm_node.tags)),
                                });

                                Pass2Node::Routing(id, point)
                            }
                        }
                    })
                    .and_then(|node| node_map.insert(osm_node.id, node));
            }

            OsmObj::Way(ref way) => {
                seen_way = true;
                if way.nodes.len() < 2 || !is_way_routeable(&way.tags) {
                    continue;
                }

                let mut accumulated_dist = 0.0;
                let mut prev_point: Option<LatLng> = None;
                let mut prev_node_id: Option<NodeIndex> = None;

                for osm_node_id in way.nodes.iter() {
                    let node = node_map
                        .get(osm_node_id)
                        .expect("Missing node info from way");

                    let point = match node {
                        Pass2Node::Routing(_, p) => p,
                        Pass2Node::Geometry(p) => p,
                    };

                    if let Some(prev_point) = prev_point {
                        accumulated_dist += prev_point.dist_to(point);
                    }
                    prev_point = Some(*point);

                    if let Pass2Node::Routing(node_id, _) = node {
                        if let Some(prev_id) = prev_node_id {
                            // Duplicate edges are possible!
                            // - https://www.openstreetmap.org/way/1060404609
                            // - https://www.openstreetmap.org/way/1060404608

                            graph.add_edge(
                                *node_id,
                                prev_id,
                                EdgeData {
                                    dist: accumulated_dist as u32,
                                    tags: EdgeTags::from(strip_tags(&way.tags)),
                                    // TODO: populate
                                    geometry: vec![],
                                },
                            );

                            accumulated_dist = 0.0;
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

    let node_coordinates: Vec<_> = graph
        .node_indices()
        .map(|ix| IndexedCoordinate::new(ix, graph[ix].point.into()))
        .collect();

    Ok(OsmGraph {
        inner: graph,
        index: SpatialIndex::build(node_coordinates),
    })
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

        use tags::HighwayKind::*;
        let mut multiple = match edge_data.tags.highway {
            Motorway | MotorwayLink => INACCESSIBLE,
            Trunk | TrunkLink => 10,
            Primary | PrimaryLink => 5,
            Path | Steps | Track => 0,
            _ => 1,
        };

        use tags::SurfaceKind::*;
        multiple += match edge_data.tags.surface {
            Unpaved(_) => 0,
            Paved(_) => 10,
            Cobblestone(_) => 20,
            tags::SurfaceKind::Unknown => 10,
        };

        let max_multiple = 1000;
        if multiple > max_multiple {
            return INACCESSIBLE;
        }

        edge_data.dist * (1 + multiple)
    }

    // TODO: Build lat,lng -> NodeIndex lookup so we don't need to pass node index values.
    fn find_route(&self, from: NodeIndex, to: NodeIndex) -> Option<Vec<LatLng>> {
        let dest_node = self.inner.node_weight(to).expect("Invalid `to` given.");

        let path = astar(
            &self.inner,
            from,
            |node_id| node_id == to,
            |e| self.score_edge(e),
            |node_id| {
                self.inner
                    .node_weight(node_id)
                    .map(|n| n.point.dist_to(&dest_node.point) as u32)
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
                        .point
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
            let geom = path.iter().map(|pt| pt.to_degrees()).collect::<Vec<_>>();

            println!(" {{ \"type\": \"Feature\", \"geometry\": {{\"type\": \"LineString\", \"coordinates\": {:?}}}, \"properties\": {{}}}}", geom);
        }
    }

    timer.elapsed("complete");

    Ok(())
}
