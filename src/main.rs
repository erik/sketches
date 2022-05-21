use std::collections::{HashMap, HashSet};

use osmpbfreader::{
    objects::{NodeId as OsmNodeId, WayId as OsmWayId},
    OsmObj, OsmPbfReader, Tags,
};
use petgraph::csr::Csr;
use petgraph::prelude::*;

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

// TODO: Simplify edges to only include juntions.
// TODO: Attach metadata to each edge
fn construct_graph() -> Result<Graph, std::io::Error> {
    let f = std::fs::File::open("./data/andorra.osm.pbf").unwrap();
    let mut pbf = OsmPbfReader::new(f);

    let mut node_counter = 0;
    let mut way_counter = 0;

    // TODO: Add Vec<WayId> as well so we can merge tags.
    // I think??
    let mut referenced_node_ids = HashMap::<OsmNodeId, NodeId>::new();

    let mut node_data = HashMap::<NodeId, NodeData>::new();
    let mut way_tags = HashMap::<WayId, Tags>::new();

    let mut edges = HashSet::<(NodeId, NodeId)>::new();

    for pass in 0..=1 {
        pbf.rewind().unwrap();

        // TODO: When would this fail?
        for obj in pbf.par_iter().map(Result::unwrap) {
            match obj {
                // On the first pass gather all valid ways, and the nodes associated.
                OsmObj::Way(ref way) if pass == 0 => {
                    // TODO: What else is significant here?
                    let is_routable =
                        way.tags.contains_key("highway") || way.tags.contains("route", "ferry");

                    if is_routable {
                        let way_id = way_counter;
                        way_counter += 1;

                        let mut prev_id = None;
                        for osm_node_id in way.nodes.iter() {
                            let node_id = match referenced_node_ids.get(osm_node_id) {
                                Some(id) => *id,
                                None => {
                                    let node_id = node_counter;
                                    referenced_node_ids.insert(*osm_node_id, node_id);
                                    node_counter += 1;
                                    node_id
                                }
                            };

                            if let Some(prev_id) = prev_id {
                                // TODO: or maybe attach way_id here?
                                edges.insert((prev_id, node_id));
                            }
                            prev_id = Some(node_id);
                        }

                        way_tags.insert(way_id, way.tags.clone());
                    }
                }

                // TODO: Which nodes do we care about individually? Barriers?
                // Only gather nodes in the second pass.
                OsmObj::Node(ref node) if pass != 0 => {
                    if let Some(node_id) = referenced_node_ids.get(&node.id) {
                        let tags = if node.tags.is_empty() {
                            None
                        } else {
                            Some(node.tags.clone())
                        };

                        node_data.insert(
                            *node_id,
                            NodeData {
                                lat: node.lat(),
                                lon: node.lon(),
                                tags,
                            },
                        );
                    }
                }

                // TODO: Support relations
                _ => (),
            }
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
