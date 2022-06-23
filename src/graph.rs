use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::From;
use std::io::{Read, Seek};
use std::path::Path;

use osmpbfreader::{
    objects::{Node as OsmNode, NodeId as OsmNodeId},
    OsmObj, OsmPbfReader, Tags,
};
use petgraph::Direction::{Incoming, Outgoing};
use petgraph::{
    algo::astar,
    graph::NodeIndex,
    graph::{EdgeReference, Graph},
    Undirected,
};
use smartstring::{Compact, SmartString};

use crate::index::{IndexedCoordinate, Point2D, SpatialIndex};
use crate::profile::{Profile, ProfileRuntime};
use crate::tags::{CompactTags, TagDict};

/// In radians
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LatLng {
    pub lat: f32,
    pub lon: f32,
}

#[derive(Debug)]
pub struct NodeData {
    point: LatLng,
    tags: CompactTags,
}

#[derive(Debug, Clone)]
pub struct EdgeData {
    // meters
    // TODO: Can likely store this as dist / 10 and fit in u16 (max: 655km)
    dist: u32,
    tags: CompactTags,
    // TODO: Can delta-encode coordinates against start point to fit in (u16, u16)
    // TODO: Alternatively - polyline, without ASCII representation
    geometry: Vec<LatLng>,
}

pub struct OsmGraph {
    // TODO: use Csr, but petgraph doesn't support parallel edges, which we need.
    // TODO: Use a directed graph so we can represent one ways etc.
    pub inner: Graph<NodeData, EdgeData, Undirected>,
    pub index: SpatialIndex<NodeIndex, Point2D>,
    pub tag_dict: TagDict<SmartString<Compact>>,
    // TODO: This doesn't belong here
    pub runtime: RefCell<ProfileRuntime>,
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
}

impl From<&OsmNode> for LatLng {
    fn from(node: &OsmNode) -> LatLng {
        let (lat, lon) = (node.lat() as f32, node.lon() as f32);

        LatLng {
            lat: lat.to_radians(),
            lon: lon.to_radians(),
        }
    }
}

impl From<LatLng> for Point2D {
    fn from(pt: LatLng) -> Point2D {
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
    R: Read + Seek,
{
    // A node is used for routing if it:
    // - is the first or last node of a way
    // - is shared by multiple ways (i.e. a junction)
    //
    // TODO: Could be a bitvector.
    let mut node_kind_mapping = HashMap::<OsmNodeId, NodeKind>::new();
    let mut processed_nodes = 0;
    let mut processed_ways = 0;

    for (i, obj) in reader.par_iter().enumerate() {
        let obj = obj.unwrap();

        if i % 1_000_000 == 0 {
            print!(
                "\r[PASS 1]: objects={:?} nodes={:?} ways={:?}",
                i, processed_nodes, processed_ways,
            );
        }

        match obj {
            OsmObj::Node(ref node) => {
                if is_node_used_for_routing(&node.tags) {
                    node_kind_mapping.insert(node.id, NodeKind::Routing);
                    processed_nodes += 1;
                }
            }

            OsmObj::Way(ref way) => {
                if way.nodes.len() < 2 || !is_way_routeable(&way.tags) {
                    continue;
                }
                processed_ways += 1;

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
        "\n[PASS 1] complete: nodes={:?} ways={:?}",
        processed_nodes, processed_ways,
    );

    node_kind_mapping
}

enum Pass2Node {
    Routing(NodeIndex<u32>, LatLng),
    Geometry(LatLng),
}

// Ramer-Douglas-Peucker line simplification
// TODO: needs tests, not checked
fn simplify(geo: &[LatLng], epsilon: f32) -> Vec<LatLng> {
    let mut result = Vec::with_capacity(geo.len());
    if geo.len() >= 2 {
        result.push(geo[0]);
        simplify_inner(geo, epsilon, &mut result);
    }
    result
}

// TODO: Probably shouldn't be working in radians here...
fn simplify_inner(geo: &[LatLng], epsilon: f32, result: &mut Vec<LatLng>) {
    if geo.len() < 2 {
        return;
    }

    let (first, last) = (geo[0], geo[geo.len() - 1]);

    let dy = last.lat - first.lat;
    let dx = last.lon - first.lon;

    let mut max_dist = 0.0;
    let mut index = 0;

    for i in 1..geo.len() - 1 {
        let p = geo[i];
        // Distance from `point` to line [first, last]
        let d = (p.lon * dy - p.lat * dx) + (last.lon * first.lat - last.lat * first.lon);
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

// TODO: Extract out some kind of helper functions, too big.
pub fn construct_graph(path: &Path) -> Result<OsmGraph, std::io::Error> {
    let f = std::fs::File::open(path).unwrap();
    let mut pbf = OsmPbfReader::new(f);

    let node_kind = construct_node_kind_map(&mut pbf);

    // Reset to start of file for next iteration.
    pbf.rewind().unwrap();

    let mut node_map = HashMap::<OsmNodeId, Pass2Node>::with_capacity(node_kind.len());
    let mut graph = Graph::<NodeData, EdgeData, _>::new_undirected();
    let mut tag_dict = TagDict::new();

    let mut processed_nodes = 0;
    let mut processed_ways = 0;
    for (i, obj) in pbf.par_iter().enumerate() {
        let obj = obj.unwrap();

        if i % 1_000_000 == 0 {
            print!(
                "\r[PASS 2]: objects={:?} nodes={:?} ways={:?}",
                i, processed_nodes, processed_ways,
            );
        }

        match obj {
            OsmObj::Relation(_) => {}

            OsmObj::Node(ref osm_node) => {
                assert!(processed_ways == 0, "input files MUST be sorted!");

                node_kind
                    .get(&osm_node.id)
                    .map(|kind| {
                        processed_nodes += 1;

                        let point = LatLng::from(osm_node);
                        match kind {
                            NodeKind::Geometry => Pass2Node::Geometry(point),
                            NodeKind::Routing => {
                                let id = graph.add_node(NodeData {
                                    point,
                                    tags: tag_dict.from_osm(&osm_node.tags),
                                });

                                Pass2Node::Routing(id, point)
                            }
                        }
                    })
                    .and_then(|node| node_map.insert(osm_node.id, node));
            }

            OsmObj::Way(ref way) => {
                if way.nodes.len() < 2 || !is_way_routeable(&way.tags) {
                    continue;
                }

                processed_ways += 1;

                let mut accumulated_dist = 0.0;
                let mut way_geo = vec![];

                let mut prev_point: Option<LatLng> = None;
                let mut prev_node_id: Option<NodeIndex> = None;

                for osm_node_id in way.nodes.iter() {
                    let node = node_map
                        .get(osm_node_id)
                        .expect("Missing node info from way");

                    let point = match node {
                        Pass2Node::Routing(_, p) => *p,
                        Pass2Node::Geometry(p) => *p,
                    };

                    way_geo.push(point);
                    if let Some(prev_point) = prev_point {
                        accumulated_dist += prev_point.dist_to(&point);
                    }
                    prev_point = Some(point);

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
                                    tags: tag_dict.from_osm(&way.tags),
                                    // Approx 1m precision at equator.
                                    geometry: simplify(&way_geo, 1e-5),
                                },
                            );

                            way_geo.clear();
                            accumulated_dist = 0.0;
                        }

                        prev_node_id = Some(*node_id);
                    }
                }
            }
        }
    }
    println!(
        "\n[PASS 2] complete: nodes={}, edges={}",
        graph.node_count(),
        graph.edge_count()
    );

    let node_coordinates: Vec<_> = graph
        .node_indices()
        .map(|ix| IndexedCoordinate::new(ix, graph[ix].point.into()))
        .collect();

    let profile = Profile::parse(
        r#"
profile "foo" {
  define {
    avoid-highways = true
  }

  way-penalty {
    define {
      paved? = [surface=paved|asphalt|concrete]
      steps? = [highway=steps]
    }

    when {
        steps? => 0
        paved? => 10
        else   => 3
    }
  }
}"#,
    )
    .expect("parse profile");
    let runtime = ProfileRuntime::from(profile).expect("load profile");

    Ok(OsmGraph {
        inner: graph,
        index: SpatialIndex::build(&node_coordinates),
        runtime: RefCell::new(runtime),
        tag_dict,
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
        let tag_source = self.tag_dict.tag_source(&edge_data.tags);
        let score = self
            .runtime
            .borrow_mut()
            .score_way(&tag_source)
            .expect("score way");

        edge_data.dist + (edge_data.dist as f64 * score as f64) as u32
    }

    pub fn find_route(&self, from: LatLng, to: LatLng) -> Option<Vec<LatLng>> {
        let from = self.index.find_nearest_within(from.into(), 500.0)?;
        let to = self.index.find_nearest_within(to.into(), 500.0)?;

        self.find_route_from_nodes(from, to)
    }

    // TODO: Build lat,lng -> NodeIndex lookup so we don't need to pass node index values.
    fn find_route_from_nodes(&self, from: NodeIndex, to: NodeIndex) -> Option<Vec<LatLng>> {
        let dest_node = self.inner.node_weight(to).expect("Invalid `to` given.");

        let (cost, path) = astar(
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
        )?;

        println!("Total Cost = {:?} km (equiv)", cost as f32 / 1000.0);

        let path_geom = self.build_geometry(&path);
        Some(path_geom)
    }

    fn build_geometry(&self, node_ids: &[NodeIndex]) -> Vec<LatLng> {
        let mut geometry = Vec::with_capacity(node_ids.len());

        for (i, &node_id) in node_ids[1..].iter().enumerate() {
            let prev_node_id = node_ids[i];

            // TODO: How do we figure out which edge was taken in the case of parallel ones?
            // There's another function: edges_connecting, which might be relevant.
            self.inner
                .find_edge_undirected(prev_node_id, node_id)
                .and_then(|(edge_idx, direction)| {
                    let edge = self.inner.edge_weight(edge_idx)?;

                    match direction {
                        Incoming => geometry.extend(&edge.geometry),
                        Outgoing => geometry.extend(edge.geometry.iter().rev()),
                    };

                    Some(())
                })
                .expect("no edge between given nodes");
        }

        geometry
    }
}
