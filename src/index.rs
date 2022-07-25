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

pub enum SnappedTo {
    Node(NodeIndex),
    Edge {
        edge_id: EdgeIndex,
        node_id: NodeIndex,
        offset: usize,
    },
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
                    .points
                    .iter()
                    .map(|ll| (ll.lon.to_degrees(), ll.lat.to_degrees()))
                    .collect::<Vec<_>>();

                let aabb = AABB::from_points(&geom);
                GeomWithData::new(Rectangle::from_aabb(aabb), edge_ref.id())
            })
            .collect::<Vec<_>>();

        Self {
            tree: RTree::bulk_load(bboxes),
        }
    }

    pub fn snap_point(
        &self,
        graph: &Graph<NodeData, EdgeData, Undirected>,
        query_pt: LatLng,
        radius_meters: f32,
    ) -> Option<SnappedTo> {
        // TODO: Clean this up, this is degrees / meter at equator.
        const DEGREES_PER_METER: f32 = 0.0000089;
        let radius_degrees = DEGREES_PER_METER * radius_meters;

        let mut snap: Option<SnappedTo> = None;
        let mut min_dist = radius_meters;

        let query = (query_pt.lon.to_degrees(), query_pt.lat.to_degrees());
        for edge in self.tree.locate_within_distance(query, radius_degrees) {
            let edge_id = edge.data;

            let edge_weight = graph
                .edge_weight(edge_id)
                .expect("index out of sync with graph");

            let (from_node_id, to_node_id) = graph
                .edge_endpoints(edge_id)
                .expect("index out of sync with graph");

            for (i, pt) in edge_weight.geometry.points.iter().enumerate() {
                let dist = query_pt.dist_to(pt);
                if dist >= min_dist {
                    continue;
                }

                min_dist = dist;

                snap = if i == 0 {
                    Some(SnappedTo::Node(from_node_id))
                } else if i == edge_weight.geometry.points.len() - 1 {
                    Some(SnappedTo::Node(to_node_id))
                } else {
                    let dist_from = pt.dist_to(&edge_weight.geometry.points[0]);
                    let dist_to = pt.dist_to(
                        &edge_weight.geometry.points[edge_weight.geometry.points.len() - 1],
                    );
                    let nearest_node = if dist_from < dist_to {
                        from_node_id
                    } else {
                        to_node_id
                    };

                    Some(SnappedTo::Edge {
                        edge_id,
                        node_id: nearest_node,
                        offset: i,
                    })
                };
            }
        }

        snap
    }
}
