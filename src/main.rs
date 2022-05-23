use std::collections::{HashMap, HashSet};

use osmpbfreader::{
    objects::{NodeId as OsmNodeId, WayId as OsmWayId},
    OsmObj, OsmPbfReader, Tags,
};
use petgraph::csr::Csr;
// use petgraph::prelude::*;

type WayId = u32;
type NodeId = u32;
type EdgeId = u32;

type Coordinate = (f32, f32);

#[derive(Debug)]
struct NodeData {
    lat: f64,
    lon: f64,
    // TODO: Extract only tags we care about
    tags: Option<Tags>,
}

#[derive(Debug)]
struct EdgeData {
    // meters
    //
    // TODO: Maybe store multiples of meters so we can fit in a u16 -
    // need to check max realistic edge length.
    dist: u32,
    tags: Option<Tags>,
    // TODO: Can delta-encode coordinates against start point to fit in (u16, u16)
    // TODO: Alternatively - polyline, without ASCII representation
    geometry: Vec<Coordinate>,
}

struct Graph {
    csr: Csr<NodeId, ()>,
    // TODO: Can be vec
    node_data: HashMap<NodeId, NodeData>,
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

fn construct_node_id_mapping<R>(reader: &mut OsmPbfReader<R>) -> HashMap<OsmNodeId, NodeId>
where
    R: std::io::Read + std::io::Seek,
{
    // A node is used for routing if it:
    // - is the first or last node of a way
    // - is shared by multiple ways (i.e. a junction)
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

    // Assign new IDs without any gaps
    return is_routing_node
        .iter()
        .filter_map(|(&osm_id, &is_routing)| {
            if is_routing {
                let id = next_id;
                next_id += 1;
                Some((osm_id, id))
            } else {
                None
            }
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
fn construct_graph() -> Result<Graph, std::io::Error> {
    let f = std::fs::File::open("./data/andorra.osm.pbf").unwrap();
    let mut pbf = OsmPbfReader::new(f);

    let node_id_mapping = construct_node_id_mapping(&mut pbf);

    // Reset to start of file for next iteration.
    pbf.rewind().unwrap();

    // TODO: Can be a Vec<> using NodeId as the index.
    let mut node_data = HashMap::<NodeId, NodeData>::new();
    let mut edges = HashMap::<(NodeId, NodeId), EdgeData>::new();

    for obj in pbf.par_iter() {
        let obj = obj.unwrap();
        match obj {
            OsmObj::Way(ref way) => {
                if way.nodes.len() < 2 || !is_way_routeable(&way.tags) {
                    continue;
                }

                // TODO: Calculate distance using geometry nodes
                // TODO: Simplify geometry nodes and store
                let routing_nodes = way
                    .nodes
                    .iter()
                    .filter_map(|osm_node_id| node_id_mapping.get(osm_node_id));

                let mut prev_id = None;
                for &node_id in routing_nodes {
                    if let Some(prev) = prev_id {
                        let key = (prev, node_id);
                        // TODO: Duplicate edges are possible! How do we handle this?
                        //   https://www.openstreetmap.org/way/1060404609
                        //   https://www.openstreetmap.org/way/1060404608
                        //
                        // For now, just go with whatever we see first...
                        if !edges.contains_key(&key) {
                            // Skip...
                            edges.insert(
                                key,
                                // TODO: populate
                                EdgeData {
                                    dist: 0,
                                    tags: strip_tags(&way.tags),
                                    geometry: vec![],
                                },
                            );
                        }
                    }

                    prev_id = Some(node_id);
                }
            }

            // TODO: We need to read nodes before ways (for lat, lng). Move this to the first pass.
            OsmObj::Node(ref node) => {
                if let Some(&node_id) = node_id_mapping.get(&node.id) {
                    node_data.insert(
                        node_id,
                        NodeData {
                            lat: node.lat(),
                            lon: node.lon(),
                            tags: strip_tags(&node.tags),
                        },
                    );
                }
            }

            // TODO: handle relations
            OsmObj::Relation(_) => {}
        }
    }

    let mut edges = edges.keys().collect::<Vec<_>>();
    edges.sort();

    let csr = Csr::from_sorted_edges(&edges).unwrap();

    println!(
        "Created CSR graph: nodes={}, edges={}",
        csr.node_count(),
        csr.edge_count()
    );

    Ok(Graph { csr, node_data })
}

fn main() -> Result<(), std::io::Error> {
    let _graph = construct_graph()?;

    Ok(())
}
