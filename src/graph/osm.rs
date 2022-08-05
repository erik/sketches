use std::collections::{HashMap, HashSet};
use std::convert::From;
use std::fs::File;
use std::io::{Error, Read, Seek};
use std::path::Path;

use osmpbfreader::{objects::NodeId as OsmNodeId, Node, OsmObj, OsmPbfReader, Tags, Way};
use petgraph::Undirected;
use petgraph::{graph::Graph, graph::NodeIndex};

use crate::geo;
use crate::geo::Point;
use crate::index::SpatialIndex;
use crate::raster::{mapper, XYZTileSampler};
use crate::tags::{CompactTags, TagDict};

use super::{EdgeData, EdgeDirection, NodeData, OsmGraph, TagDictId};

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

#[inline]
fn has_one_of(tags: &Tags, k: &str, vals: &[&str]) -> bool {
    tags.get(k)
        .map_or(false, |val| vals.contains(&val.as_str()))
}

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
    way.tags.contains_key("junction")
}

#[derive(Copy, Clone)]
enum NodeKind {
    Routing,
    Geometry,
}

struct SecondPassNode {
    point: Point,
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

struct TagDictDeduper<'a> {
    tag_dict: &'a mut TagDict,
    tags: HashSet<CompactTags>,
}

impl<'a> TagDictDeduper<'a> {
    fn new(tag_dict: &'a mut TagDict) -> Self {
        Self {
            tag_dict,
            tags: HashSet::new(),
        }
    }

    fn insert(&mut self, tags: &Tags) -> TagDictId {
        let tags = self.tag_dict.from_osm(tags);
        self.tags.insert(tags);

        TagDictId(self.tags.len() - 1)
    }

    fn collect(&mut self) -> Vec<CompactTags> {
        let tags = std::mem::take(&mut self.tags);
        tags.into_iter().collect()
    }
}

impl From<&Node> for Point {
    fn from(node: &Node) -> Self {
        Point {
            lat: node.lat() as f32,
            lng: node.lon() as f32,
        }
    }
}

struct OsmGraphBuilder<'a, 'b, R> {
    reader: OsmPbfReader<R>,
    node_kind: HashMap<OsmNodeId, NodeKind>,
    graph: Graph<NodeData, EdgeData, Undirected>,
    tags: &'a mut TagDictDeduper<'b>,
}

impl<'a, 'b, R> OsmGraphBuilder<'a, 'b, R>
where
    R: Read + Seek,
{
    fn new(reader: OsmPbfReader<R>, tags: &'a mut TagDictDeduper<'b>) -> Self {
        OsmGraphBuilder {
            reader,
            tags,
            node_kind: HashMap::new(),
            graph: Graph::new_undirected(),
        }
    }

    fn load_graph(mut self) -> Result<Graph<NodeData, EdgeData, Undirected>, Error> {
        self.categorize_nodes()?;
        self.construct_graph()?;

        Ok(self.graph)
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

        let global_heat = XYZTileSampler::new(
            "https://strava-heatmap-proxy.rkprc.workers.dev/global/orange/all/{z}/{x}/{y}.png",
            Path::new("./data/tiles/global/"),
        );
        let personal_heat = XYZTileSampler::new(
            "https://strava-heatmap-proxy.rkprc.workers.dev/personal/orange/all/{z}/{x}/{y}.png",
            Path::new("./data/tiles/personal/"),
        );

        for obj in self.reader.par_iter() {
            let obj = obj.expect("encountered unexpected object reading PBF");
            trk.object();

            match obj {
                OsmObj::Node(node) => {
                    assert!(trk.ways == 0, "input file not sorted (saw NODE after WAY)");

                    if let Some(&kind) = self.node_kind.get(&node.id) {
                        trk.node();
                        let point = Point::from(&node);
                        let index = match kind {
                            NodeKind::Geometry => None,
                            NodeKind::Routing => Some(self.graph.add_node(NodeData {
                                point,
                                tag_id: self.tags.insert(&node.tags),
                            })),
                        };

                        node_map.insert(node.id, SecondPassNode { point, index });
                    }
                }

                OsmObj::Way(way) if is_routable_way(&way) => {
                    trk.way();

                    let mut dist = 0.0;
                    let mut way_geo: Vec<Point> = vec![];

                    let mut prev_node_id: Option<NodeIndex> = None;

                    for id in way.nodes.iter() {
                        let node = node_map.get(id).expect("Missing node info from way");

                        if let Some(prev) = way_geo.last() {
                            dist += prev.haversine(&node.point);
                        }

                        way_geo.push(node.point);

                        if let Some(index) = node.index {
                            if let Some(prev_id) = prev_node_id {
                                // Duplicate edges are possible!
                                // - https://www.openstreetmap.org/way/1060404609
                                // - https://www.openstreetmap.org/way/1060404608

                                let geometry = geo::simplify_line(&way_geo, 1e-6);

                                self.graph.add_edge(
                                    index,
                                    prev_id,
                                    EdgeData {
                                        tag_id: self.tags.insert(&way.tags),
                                        direction: EdgeDirection::infer(&way.tags),
                                        distance: dist as u32,
                                        popularity_global: global_heat
                                            .sample(&geometry, mapper::strava_orange)
                                            .map(|values| {
                                                values.iter().map(|x| x.unwrap_or(0.0)).sum::<f32>()
                                                    / geometry.len() as f32
                                            })
                                            .unwrap(),
                                        popularity_self: personal_heat
                                            .sample(&geometry, mapper::strava_orange)
                                            .map(|values| {
                                                values.iter().map(|x| x.unwrap_or(0.0)).sum::<f32>()
                                                    / geometry.len() as f32
                                            })
                                            .unwrap(),
                                        points: geometry,
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

        // TODO: fixed tag dict to avoid useless k/v pairs
        let mut tag_dict = TagDict::new();
        let mut deduper = TagDictDeduper::new(&mut tag_dict);

        let graph = OsmGraphBuilder::new(reader, &mut deduper).load_graph()?;
        let index = SpatialIndex::build(&graph);

        let tag_sets = deduper.collect();

        println!("distinct tag_sets = {}", tag_sets.len());
        // for t in tag_sets.iter() {
        //     println!("t: {:?}", t.decode(&tag_dict));
        // }

        Ok(OsmGraph {
            inner: graph,
            index,
            tag_dict,
            tag_sets,
        })
    }
}

impl EdgeDirection {
    // TODO: This seriously needs a review
    fn infer(tags: &Tags) -> Self {
        let is_oneway = tags.contains("junction", "roundabout")
            || has_one_of(tags, "oneway", &["yes", "-1"])
            || has_one_of(tags, "oneway:bicycle", &["yes", "-1"])
            || tags.contains_key("vehicle:backward")
            || tags.contains_key("bicycle:backward")
            || tags.contains_key("vehicle:forward")
            || tags.contains_key("bicycle:forward");

        let is_bike_exempt = tags.contains("oneway:bicycle", "no")
            || tags.contains("bicycle:backward", "yes")
            || has_one_of(tags, "cycleway", &["opposite", "opposite_lane"])
            || has_one_of(tags, "cycleway:left", &["opposite", "opposite_lane"])
            || has_one_of(tags, "cycleway:right", &["opposite", "opposite_lane"]);

        if is_oneway && !is_bike_exempt {
            let is_reversed = tags.contains("oneway", "-1")
                || tags.contains("oneway:bicycle", "-1")
                || tags.contains("vehicle:forward", "no")
                || tags.contains("bicycle:forward", "no");

            if is_reversed {
                EdgeDirection::Reversed
            } else {
                EdgeDirection::Forward
            }
        } else {
            EdgeDirection::Both
        }
    }
}
