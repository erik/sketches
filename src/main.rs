use std::collections::HashMap;

use osmpbfreader::{
    objects::{NodeId as OsmNodeId, WayId as OsmWayId},
    OsmObj, OsmPbfReader, Tags,
};
use petgraph::{algo::astar, csr::Csr, visit::EdgeRef};

type NodeId = u32;
type EdgeId = (NodeId, NodeId);

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

#[derive(Debug)]
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
    csr: Csr<NodeId, ()>,
    // TODO: Can be vec
    node_data: HashMap<NodeId, NodeData>,
    edge_data: HashMap<EdgeId, EdgeData>,
}

impl Coordinate {
    #[inline(always)]
    fn to_radians(&self) -> (f32, f32) {
        (self.lat.to_radians(), self.lon.to_radians())
    }

    // Haversine, returns meters
    // TODO: unchecked
    fn dist_to(&self, other: &Coordinate) -> u32 {
        let (lat1, lon1) = self.to_radians();
        let (lat2, lon2) = other.to_radians();

        let dt_lon = lon2 - lon1;
        let dt_lat = lat2 - lat1;

        let a = (dt_lat / 2.0_f32).sin().powi(2)
            + lat1.cos() * lat2.cos() * (dt_lon / 2.0_f32).sin().powi(2);
        let c = 2.0_f32 * a.sqrt().asin();
        return (6367_000_f32 * c) as u32;
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

enum FirstPassNode {
    Routing(NodeId),
    Geometry,
}

fn construct_node_id_mapping<R>(reader: &mut OsmPbfReader<R>) -> HashMap<OsmNodeId, FirstPassNode>
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

    let mut next_id = 0;

    // TODO: Could store as sorted Vec<> and use binary search rather
    // than constructing hashmap.
    //
    //Assign new IDs without any gaps
    return is_routing_node
        .iter()
        .map(|(&osm_id, &is_routing)| {
            let node = if is_routing {
                let id = next_id;
                next_id += 1;
                FirstPassNode::Routing(id)
            } else {
                FirstPassNode::Geometry
            };

            (osm_id, node)
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

// TODO: we can avoid hashmaps and the ID assignment counters by using a bitmap
// TODO: Attach metadata to each edge
fn construct_graph() -> Result<OsmGraph, std::io::Error> {
    let f = std::fs::File::open("./data/andorra.osm.pbf").unwrap();
    let mut pbf = OsmPbfReader::new(f);

    let node_id_mapping = construct_node_id_mapping(&mut pbf);

    // Reset to start of file for next iteration.
    pbf.rewind().unwrap();

    // TODO: Can be a Vec<> using NodeId as the index.
    let mut node_geom = HashMap::<OsmNodeId, Coordinate>::with_capacity(node_id_mapping.len());
    let mut node_data = HashMap::<NodeId, NodeData>::new();

    let mut edge_data = HashMap::<(NodeId, NodeId), EdgeData>::new();

    let mut seen_way = false;
    for obj in pbf.par_iter() {
        let obj = obj.unwrap();
        match obj {
            OsmObj::Relation(_) => {}

            OsmObj::Node(ref osm_node) => {
                assert!(seen_way == false, "input files MUST be sorted!");

                if let Some(node) = node_id_mapping.get(&osm_node.id) {
                    let coord = Coordinate {
                        lat: osm_node.lat() as f32,
                        lon: osm_node.lon() as f32,
                    };

                    // TODO: Remove deduplication
                    node_geom.insert(osm_node.id, coord);

                    if let FirstPassNode::Routing(id) = node {
                        node_data.insert(
                            *id,
                            NodeData {
                                coord,
                                tags: strip_tags(&osm_node.tags),
                            },
                        );
                    }
                }
            }

            OsmObj::Way(ref way) => {
                seen_way = true;
                if way.nodes.len() < 2 || !is_way_routeable(&way.tags) {
                    continue;
                }

                let mut accumulated_dist = 0_u32;
                let mut prev_coord: Option<Coordinate> = None;
                let mut prev_node_id: Option<NodeId> = None;

                for osm_node_id in way.nodes.iter() {
                    let node = node_id_mapping
                        .get(osm_node_id)
                        .expect("Missing node info from way");

                    let coord = node_geom.get(osm_node_id).expect("missing node geometry");
                    if let Some(prev_coord) = prev_coord {
                        accumulated_dist += prev_coord.dist_to(coord);
                    }
                    prev_coord = Some(*coord);

                    if let FirstPassNode::Routing(node_id) = node {
                        if let Some(prev_id) = prev_node_id {
                            let key = (prev_id, *node_id);
                            // TODO: Duplicate edges are possible! How do we handle this in CSR?
                            //   https://www.openstreetmap.org/way/1060404609
                            //   https://www.openstreetmap.org/way/1060404608
                            //
                            // For now, just go with whatever we see first...
                            if edge_data.contains_key(&key) {
                                continue;
                            }

                            edge_data.insert(
                                key,
                                EdgeData {
                                    dist: accumulated_dist,
                                    tags: strip_tags(&way.tags),
                                    // TODO: populate
                                    geometry: vec![],
                                },
                            );
                        }

                        prev_node_id = Some(*node_id);
                    }
                }
            }
        }
    }

    let mut edges = edge_data.keys().collect::<Vec<_>>();
    edges.sort();

    let csr = Csr::from_sorted_edges(&edges).unwrap();

    println!(
        "Created CSR graph: nodes={}, edges={}",
        csr.node_count(),
        csr.edge_count()
    );

    Ok(OsmGraph {
        csr,
        node_data,
        edge_data,
    })
}

fn main() -> Result<(), std::io::Error> {
    let graph = construct_graph()?;

    for from in 0..100 {
        for to in 1..100 {
            let path = astar(
                &graph.csr,
                from,
                |node_id| node_id == to,
                |edge_ref| {
                    let key = (edge_ref.source(), edge_ref.target());
                    let edge = graph.edge_data.get(&key).expect("invalid edge");
                    edge.dist
                },
                |_node_id| {
                    // TODO: haversine(node, to) * distance multiple?
                    0
                },
            );

            if let Some((cost, ref path)) = path {
                println!("Path: {:?}", path);
                let geom = path
                    .iter()
                    .map(|node_id| graph.node_data.get(&node_id).expect("invalid from_node_id"))
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
