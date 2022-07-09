use std::collections::HashMap;
use std::convert::From;
use std::fs::File;
use std::io::{Error, Read, Seek};
use std::path::Path;

use osmpbfreader::{objects::NodeId as OsmNodeId, Node, OsmObj, OsmPbfReader, Way};
use petgraph::Undirected;
use petgraph::{graph::Graph, graph::NodeIndex};

use crate::graph::{EdgeData, LatLng, NodeData, OsmGraph};
use crate::index::{IndexedCoordinate, SpatialIndex};
use crate::tags::{CompactString, TagDict};

const WAY_PERMITTED_ACCESS_VALUES: &[&str] =
    &["yes", "permissive", "designated", "destination", "public"];
const WAY_UNROUTABLE_HIGHWAY_VALUES: &[&str] =
    &["bus_guideway", "raceway", "proposed", "construction"];

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

fn is_routable_node(node: &Node) -> bool {
    match node.tags.get("highway") {
        Some(val) if NODE_HIGHWAY_ROUTING_VALUES.contains(&val.as_str()) => return true,
        Some(_) => {}
        None => {}
    }

    return NODE_ROUTING_TAGS.iter().any(|&t| node.tags.contains_key(t));
}

/// Return if the OSM Way has tags that are relevant to routability
///
/// Reference: https://wiki.openstreetmap.org/wiki/OSM_tags_for_routing
fn is_routable_way(way: &Way) -> bool {
    if way.nodes.len() < 2 {
        return false;
    }

    if way.tags.contains("route", "ferry") {
        return true;
    }

    // TODO: What about something like access=no + bicycle=yes?
    match way.tags.get("access") {
        Some(val) if !WAY_PERMITTED_ACCESS_VALUES.contains(&val.as_str()) => return false,
        Some(_) => {}
        None => {}
    }

    match way.tags.get("highway") {
        Some(val) if WAY_UNROUTABLE_HIGHWAY_VALUES.contains(&val.as_str()) => return false,
        Some(_) => return true,
        None => {}
    };

    // If we don't have a highway=*, we need to at least have a junction
    return way.tags.contains_key("junction");
}

#[derive(Copy, Clone)]
enum NodeKind {
    Routing,
    Geometry,
}

struct SecondPassNode {
    point: LatLng,
    index: Option<NodeIndex<u32>>,
}

struct ProgressTracker {
    stage: &'static str,
    objects: usize,
    nodes: usize,
    ways: usize,
}

impl ProgressTracker {
    fn new(stage: &'static str) -> Self {
        println!();
        Self {
            stage,
            objects: 0,
            nodes: 0,
            ways: 0,
        }
    }

    #[inline]
    fn report(&self, state: &'static str) {
        let start_of_line = '\r';
        let previous_line = "\x1B[A";
        let erase_line = "\x1B[2K";
        println!(
            "{}{}{}[{}] {} objects={}\tnodes={}\tways={}",
            start_of_line,
            previous_line,
            erase_line,
            state,
            self.stage,
            self.objects,
            self.nodes,
            self.ways
        );
    }

    fn finish(&self) {
        self.report("DONE");
    }

    #[inline]
    fn object(&mut self) {
        self.objects += 1;

        if self.objects % 1_000_000 == 0 {
            self.report("running");
        }
    }

    #[inline]
    fn node(&mut self) {
        self.nodes += 1;
    }

    #[inline]
    fn way(&mut self) {
        self.ways += 1;
    }
}

struct OsmGraphBuilder<R> {
    reader: OsmPbfReader<R>,
    node_kind: HashMap<OsmNodeId, NodeKind>,
    graph: Graph<NodeData, EdgeData, Undirected>,
    tag_dict: TagDict<CompactString>,
}

impl<R> OsmGraphBuilder<R>
where
    R: Read + Seek,
{
    fn new(reader: OsmPbfReader<R>) -> Self {
        OsmGraphBuilder {
            reader,
            node_kind: HashMap::new(),
            graph: Graph::new_undirected(),
            tag_dict: TagDict::new(),
        }
    }

    fn load_graph(
        mut self,
    ) -> Result<
        (
            Graph<NodeData, EdgeData, Undirected>,
            TagDict<CompactString>,
        ),
        Error,
    > {
        self.categorize_nodes()?;
        self.construct_graph()?;

        // TODO: Change this, super weird response format
        Ok((self.graph, self.tag_dict))
    }

    fn categorize_nodes(&mut self) -> Result<(), Error> {
        let mut trk = ProgressTracker::new("PASS1");

        for obj in self.reader.par_iter() {
            let obj = obj.expect("encountered unexpected object reading PBF");
            trk.object();

            match obj {
                OsmObj::Node(node) if is_routable_node(&node) => {
                    trk.node();

                    self.node_kind.insert(node.id, NodeKind::Routing);
                }

                OsmObj::Way(way) if is_routable_way(&way) => {
                    trk.way();

                    for (i, &osm_node_id) in way.nodes.iter().enumerate() {
                        match self.node_kind.get_mut(&osm_node_id) {
                            Some(kind) => {
                                // If there's an existing entry, it's a either a
                                // junction or already a routing node.
                                *kind = NodeKind::Routing;
                            }
                            None => {
                                let is_first_or_last = i == 0 || i == way.nodes.len() - 1;
                                let kind = if is_first_or_last {
                                    NodeKind::Routing
                                } else {
                                    NodeKind::Geometry
                                };

                                self.node_kind.insert(osm_node_id, kind);
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        // Reset to start of file for next iteration.
        self.reader
            .rewind()
            .expect("Failed to rewind to start of file");

        trk.finish();

        Ok(())
    }

    fn construct_graph(&mut self) -> Result<(), Error> {
        let mut trk = ProgressTracker::new("PASS2");

        let mut node_map =
            HashMap::<OsmNodeId, SecondPassNode>::with_capacity(self.node_kind.len());

        for obj in self.reader.par_iter() {
            let obj = obj.expect("encountered unexpected object reading PBF");
            trk.object();

            match obj {
                OsmObj::Node(node) => {
                    assert!(trk.ways == 0, "input file not sorted (saw NODE after WAY)");

                    if let Some(&kind) = self.node_kind.get(&node.id) {
                        trk.node();
                        let point = LatLng::from(&node);
                        let index = match kind {
                            NodeKind::Geometry => None,
                            NodeKind::Routing => Some(self.graph.add_node(NodeData {
                                point,
                                tags: self.tag_dict.from_osm(&node.tags),
                            })),
                        };

                        node_map.insert(node.id, SecondPassNode { point, index });
                    }
                }

                OsmObj::Way(way) if is_routable_way(&way) => {
                    trk.way();

                    let mut dist = 0.0;
                    let mut way_geo: Vec<LatLng> = vec![];

                    let mut prev_node_id: Option<NodeIndex> = None;

                    for id in way.nodes.iter() {
                        let node = node_map.get(id).expect("Missing node info from way");

                        if let Some(prev) = way_geo.last() {
                            dist += prev.dist_to(&node.point);
                        }

                        way_geo.push(node.point);

                        if let Some(index) = node.index {
                            if let Some(prev_id) = prev_node_id {
                                // Duplicate edges are possible!
                                // - https://www.openstreetmap.org/way/1060404609
                                // - https://www.openstreetmap.org/way/1060404608

                                self.graph.add_edge(
                                    index,
                                    prev_id,
                                    EdgeData {
                                        dist: dist as u32,
                                        tags: self.tag_dict.from_osm(&way.tags),
                                        // Approx 1m precision at equator.
                                        geometry: simplify(&way_geo, 1e-5),
                                    },
                                );

                                way_geo.clear();
                                way_geo.push(node.point);
                                dist = 0.0;
                            }

                            prev_node_id = Some(index);
                        }
                    }
                }

                _ => {}
            }
        }

        trk.finish();

        Ok(())
    }
}

impl OsmGraph {
    pub fn from_osm(path: &Path) -> Result<OsmGraph, std::io::Error> {
        let file = File::open(path)?;
        let reader = OsmPbfReader::new(file);

        let (graph, tag_dict) = OsmGraphBuilder::new(reader).load_graph()?;
        let index = SpatialIndex::build(
            &graph
                .node_indices()
                .map(|ix| IndexedCoordinate::new(ix, graph[ix].point.into()))
                .collect::<Vec<_>>(),
        );

        Ok(OsmGraph {
            inner: graph,
            index,
            tag_dict,
        })
    }
}

// Ramer-Douglas-Peucker line simplification
// TODO: needs tests, not checked
fn simplify(geo: &[LatLng], epsilon: f32) -> Vec<LatLng> {
    let mut result = Vec::with_capacity(geo.len());
    result.push(geo[0]);
    simplify_inner(geo, epsilon, &mut result);
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
