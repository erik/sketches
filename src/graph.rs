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

use crate::index::{IndexedCoordinate, Point2D, SpatialIndex};
use crate::tags::{EdgeTags, NodeTags};

/// In radians
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LatLng {
    pub lat: f32,
    pub lon: f32,
}

#[derive(Debug)]
pub struct NodeData {
    point: LatLng,
    tags: NodeTags,
}

#[derive(Debug, Clone)]
pub struct EdgeData {
    // meters
    // TODO: Can likely store this as dist / 10 and fit in u16 (max: 655km)
    dist: u32,
    tags: EdgeTags,
    // TODO: Can delta-encode coordinates against start point to fit in (u16, u16)
    // TODO: Alternatively - polyline, without ASCII representation
    // TODO: Simplify geometry before storing
    geometry: Vec<LatLng>,
}

pub struct OsmGraph {
    // TODO: use Csr, but petgraph doesn't support parallel edges, which we need.
    // TODO: Use a directed graph so we can represent one ways etc.
    pub inner: Graph<NodeData, EdgeData, Undirected>,
    pub index: SpatialIndex<NodeIndex, Point2D>,
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
pub fn construct_graph(path: &Path) -> Result<OsmGraph, std::io::Error> {
    let f = std::fs::File::open(path).unwrap();
    let mut pbf = OsmPbfReader::new(f);

    let node_kind = construct_node_kind_map(&mut pbf);

    // Reset to start of file for next iteration.
    pbf.rewind().unwrap();

    let mut node_map = HashMap::<OsmNodeId, Pass2Node>::with_capacity(node_kind.len());
    let mut graph = Graph::<NodeData, EdgeData, _>::new_undirected();

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
                                    tags: NodeTags::from(strip_tags(&osm_node.tags)),
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
                                    tags: EdgeTags::from(strip_tags(&way.tags)),
                                    geometry: way_geo.clone(), //simplify(&way_geo, 0.000001),
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

    Ok(OsmGraph {
        inner: graph,
        index: SpatialIndex::build(&node_coordinates),
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

        use crate::tags::HighwayKind::*;
        let mut multiple = match edge_data.tags.highway {
            Motorway | MotorwayLink => INACCESSIBLE,
            Trunk | TrunkLink => 10,
            Primary | PrimaryLink => 5,
            Path | Steps | Track => 0,
            _ => 1,
        };

        use crate::tags::SurfaceKind::*;
        multiple += match edge_data.tags.surface {
            Unpaved(_) => 0,
            Paved(_) => 10,
            Cobblestone(_) => 20,
            crate::tags::SurfaceKind::Unknown => 10,
        };

        let max_multiple = 1000;
        if multiple > max_multiple {
            return INACCESSIBLE;
        }

        edge_data.dist * (1 + multiple)
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
