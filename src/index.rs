use petgraph::{
    graph::{EdgeIndex, Graph, NodeIndex},
    visit::EdgeRef,
    Undirected,
};
use rstar::primitives::{GeomWithData, Rectangle};
use rstar::{RTree, AABB};

use crate::graph::{EdgeData, LatLng, NodeData};

pub type RTreePoint = (f32, f32);
pub type IndexedRect<I> = GeomWithData<Rectangle<RTreePoint>, I>;

pub struct SpatialIndex {
    tree: RTree<IndexedRect<EdgeIndex>>,
}

impl SpatialIndex {
    pub fn build(graph: &Graph<NodeData, EdgeData, Undirected>) -> Self {
        let bboxes = graph
            .edge_references()
            .map(|edge_ref| {
                // TODO: Remove unnecessary mapping/collection
                let geom = edge_ref
                    .weight()
                    .geometry
                    .iter()
                    .map(|ll| (ll.lon, ll.lat))
                    .collect::<Vec<_>>();

                let aabb = AABB::from_points(&geom);
                GeomWithData::new(Rectangle::from_aabb(aabb), edge_ref.id())
            })
            .collect::<Vec<_>>();

        Self {
            tree: RTree::bulk_load(bboxes),
        }
    }

    pub fn find_nearest_within(
        &self,
        graph: &Graph<NodeData, EdgeData, Undirected>,
        point: LatLng,
        radius: f32,
    ) -> Option<(NodeIndex, EdgeIndex)> {
        let mut closest = (f32::MAX, None, None);
        let query = (point.lon, point.lat);

        for edge in self.tree.locate_within_distance(query, radius * radius) {
            let edge_id = edge.data;
            let edge_weight = graph
                .edge_weight(edge_id)
                .expect("index out of sync with graph");

            for edge_pt in &edge_weight.geometry {
                let dist = point.dist_to(edge_pt);
                if dist < closest.0 {
                    closest = (dist, Some(edge_pt), Some(edge_id));
                }
            }
        }

        match closest {
            (_, Some(_pt), Some(edge_id)) => {
                let (from_node_id, to_node_id) = graph
                    .edge_endpoints(edge_id)
                    .expect("index out of sync with graph");

                let from = graph
                    .node_weight(from_node_id)
                    .expect("index out of sync with graph");
                let to = graph
                    .node_weight(to_node_id)
                    .expect("index out of sync with graph");

                let from_dist = from.point.dist_to(&point);
                let to_dist = to.point.dist_to(&point);

                if from_dist < to_dist {
                    Some((from_node_id, edge_id))
                } else {
                    Some((to_node_id, edge_id))
                }
            }

            _ => None,
        }
    }
}
