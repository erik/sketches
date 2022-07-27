use petgraph::{
    graph::{EdgeIndex, Graph, NodeIndex},
    visit::EdgeRef,
    Undirected,
};
use rstar::primitives::{GeomWithData, Rectangle};
use rstar::{RTree, AABB};

use crate::geo::{flat::Ruler, Point};
use crate::graph::{EdgeData, NodeData};

pub type IndexedRect<I> = GeomWithData<Rectangle<Point>, I>;

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

impl rstar::Point for Point {
    type Scalar = f32;
    const DIMENSIONS: usize = 2;

    fn generate(mut generator: impl FnMut(usize) -> Self::Scalar) -> Self {
        Point {
            lng: generator(0),
            lat: generator(1),
        }
    }

    fn nth(&self, index: usize) -> Self::Scalar {
        match index {
            0 => self.lng,
            1 => self.lat,
            _ => unreachable!(),
        }
    }

    fn nth_mut(&mut self, index: usize) -> &mut Self::Scalar {
        match index {
            0 => &mut self.lng,
            1 => &mut self.lat,
            _ => unreachable!(),
        }
    }
}

impl SpatialIndex {
    pub fn build(graph: &Graph<NodeData, EdgeData, Undirected>) -> Self {
        let bboxes = graph
            .edge_references()
            .map(|edge_ref| {
                let geometry = edge_ref.weight().geometry.points.clone();
                let aabb = AABB::from_points(&geometry);
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
        query: Point,
        radius_meters: f32,
    ) -> Option<SnappedTo> {
        let mut snap: Option<SnappedTo> = None;
        let mut min_dist = radius_meters;

        let ruler = Ruler::for_point(&query);
        let radius = ruler.meters_to_deg(radius_meters);

        for edge in self.tree.locate_within_distance(query, radius) {
            let edge_id = edge.data;

            let edge_weight = graph
                .edge_weight(edge_id)
                .expect("index out of sync with graph");

            let (from_node_id, to_node_id) = graph
                .edge_endpoints(edge_id)
                .expect("index out of sync with graph");

            for (i, pt) in edge_weight.geometry.points.iter().enumerate() {
                let dist = ruler.dist_cheap(&query, pt);

                if dist >= min_dist {
                    continue;
                }

                min_dist = dist;

                snap = if i == 0 {
                    Some(SnappedTo::Node(from_node_id))
                } else if i == edge_weight.geometry.points.len() - 1 {
                    Some(SnappedTo::Node(to_node_id))
                } else {
                    let (from, to) = (
                        &edge_weight.geometry.points[0],
                        &edge_weight.geometry.points[edge_weight.geometry.points.len() - 1],
                    );

                    let dist_from = ruler.dist_cheap(pt, from);
                    let dist_to = ruler.dist_cheap(pt, to);

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
