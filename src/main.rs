use std::collections::{HashMap, HashSet};

use osmpbfreader::{
    objects::{NodeId as OsmNodeId, WayId as OsmWayId},
    OsmObj, OsmPbfReader, Tags,
};
use petgraph::csr::Csr;
// use petgraph::prelude::*;

type WayId = u32;
type NodeId = u32;

#[derive(Debug)]
struct NodeData {
    lat: f64,
    lon: f64,
    // TODO: Extract only tags we care about
    tags: Option<Tags>,
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

type Coordinate = (f32, f32);

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

        if let OsmObj::Way(ref way) = obj {
            if way.nodes.len() < 2 || !is_way_routeable(&way.tags) {
                continue;
            }

            for (i, &osm_node_id) in way.nodes.iter().enumerate() {
                is_routing_node
                    .entry(osm_node_id)
                    .and_modify(|it| *it = true)
                    .or_insert(i == 0 || i == way.nodes.len() - 1);
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

// TODO: we can avoid hashmaps and the ID assignment counters by using a bitmap
// TODO: Simplify edges to only include juntions.
// TODO: Attach metadata to each edge
fn construct_graph() -> Result<Graph, std::io::Error> {
    let f = std::fs::File::open("./data/andorra.osm.pbf").unwrap();
    let mut pbf = OsmPbfReader::new(f);

    let mut node_data = HashMap::<NodeId, NodeData>::new();
    let mut way_tags = HashMap::<WayId, Tags>::new();

    let mut edges = HashSet::<(NodeId, NodeId)>::new();

    let node_id_mapping = construct_node_id_mapping(&mut pbf);

    println!("Num routing nodes: {}", node_id_mapping.len());

    // Reset to start of file for next iteration.
    pbf.rewind().unwrap();

    for obj in pbf.par_iter().map(Result::unwrap) {
        match obj {
            OsmObj::Way(ref way) => {
                if way.nodes.len() < 2 || !is_way_routeable(&way.tags) {
                    continue;
                }

                let routing_nodes = way
                    .nodes
                    .iter()
                    .filter_map(|osm_node_id| node_id_mapping.get(osm_node_id));

                let mut prev_node = None;
                for &node in routing_nodes {
                    if let Some(prev) = prev_node {
                        edges.insert((prev, node));
                    }

                    prev_node = Some(node);
                }
            }
            OsmObj::Node(ref node) => {
                let osm_node_id = node.id;

                if let Some(&node_id) = node_id_mapping.get(&osm_node_id) {
                    let tags = if node.tags.is_empty() {
                        None
                    } else {
                        Some(node.tags.clone())
                    };

                    node_data.insert(
                        node_id,
                        NodeData {
                            lat: node.lat(),
                            lon: node.lon(),
                            tags,
                        },
                    );
                }
            }

            // TODO: handle relations
            OsmObj::Relation(_) => {}
        }
    }

    let mut edges = edges.iter().collect::<Vec<_>>();
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
