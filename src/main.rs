use std::collections::HashMap;

use osmpbfreader::{objects::NodeId as OsmNodeId, OsmObj, OsmPbfReader, Tags};
use petgraph::{algo::astar, graph::Graph, graph::NodeIndex};

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
    inner: Graph<NodeData, EdgeData>,
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
fn construct_graph() -> Result<OsmGraph, std::io::Error> {
    let f = std::fs::File::open("./data/andorra.osm.pbf").unwrap();
    let mut pbf = OsmPbfReader::new(f);

    let node_kind = construct_node_kind_map(&mut pbf);

    // Reset to start of file for next iteration.
    pbf.rewind().unwrap();

    let mut node_map = HashMap::<OsmNodeId, Pass2Node>::with_capacity(node_kind.len());

    let mut graph = Graph::<NodeData, EdgeData>::new();

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

fn main() -> Result<(), std::io::Error> {
    let graph = construct_graph()?;

    for from in 0..900 {
        for to in 8000..10000 {
            let path = astar(
                &graph.inner,
                NodeIndex::new(from),
                |node_id| node_id == NodeIndex::new(to),
                |edge| edge.weight().dist,
                |_node_id| {
                    // TODO: haversine(node, to) * distance multiple?
                    0
                },
            );

            if let Some((cost, ref path)) = path {
                println!("TotalDist={:?}, Path: {:?}", cost as f32 / 1000.0, path);
                let geom = path
                    .iter()
                    .map(|node_id| {
                        // TODO: How do we figure out which edge was taken in the case of parallel ones?
                        graph
                            .inner
                            .node_weight(*node_id)
                            .expect("invalid from_node_id")
                    })
                    .map(|node| [node.coord.lon, node.coord.lat])
                    .collect::<Vec<_>>();

                println!(
                    "
{{ \"type\": \"Feature\",
  \"geometry\": {{
    \"type\": \"LineString\",
    \"coordinates\": {:?}
  }},
  \"properties\": {{}}
}}",
                    geom
                );

                return Ok(());
            }
        }
    }

    Ok(())
}
