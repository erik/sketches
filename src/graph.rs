use petgraph::visit::EdgeRef;
use petgraph::Direction::{Incoming, Outgoing};
use petgraph::{
    algo::astar,
    graph::{EdgeIndex, NodeIndex},
    graph::{EdgeReference, Graph},
    Undirected,
};

use crate::geo::flat::Ruler;
use crate::geo::Point;
use crate::index::{SnappedTo, SpatialIndex};
use crate::profile::Runtime;
use crate::tags::{CompactTags, TagDict};

pub mod osm;

#[derive(Debug)]
pub struct NodeData {
    pub point: Point,
    tag_id: TagDictId,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EdgeDirection {
    Forward,
    Reversed,
    Both,
}

#[derive(Debug, Clone)]
pub struct EdgeData {
    tag_id: TagDictId,
    #[allow(unused)]
    direction: EdgeDirection,
    pub distance: u32,
    pub points: Vec<Point>,
    pub popularity_global: f32,
    pub popularity_self: f32,
}

#[derive(Copy, Clone, Debug)]
pub struct TagDictId(usize);

pub struct OsmGraph {
    tag_sets: Vec<CompactTags>,
    // TODO: Can delta-encode coordinates against start point to fit in (u16, u16)
    // TODO: Alternatively - polyline, without ASCII representation

    // TODO: Use a directed graph so we can represent one ways etc.
    pub inner: Graph<NodeData, EdgeData, Undirected>,
    pub index: SpatialIndex,
    pub tag_dict: TagDict,
}

impl OsmGraph {
    fn node_data(&self, id: NodeIndex) -> &NodeData {
        self.inner
            .node_weight(id)
            .expect("edge source node not in graph")
    }

    fn edge_data(&self, id: EdgeIndex) -> &EdgeData {
        self.inner
            .edge_weight(id)
            .expect("edge source node not in graph")
    }

    fn tags(&self, id: TagDictId) -> &CompactTags {
        &self.tag_sets[id.0]
    }

    fn score_edge(&self, rt: &Runtime, edge_ref: EdgeReference<'_, EdgeData>) -> f32 {
        let edge = self.edge_data(edge_ref.id());

        let edge_tags = self.tags(edge.tag_id);
        let source_tags = self.tags(self.node_data(edge_ref.source()).tag_id);
        let target_tags = self.tags(self.node_data(edge_ref.target()).tag_id);

        let global_lookup = |key: &str| match key {
            "way.length" => Some(edge.distance as f32),
            "way.popularity-global" => todo!(),
            "way.popularity-local" => todo!(),
            _ => None,
        };

        // TODO: we can cache the score for a given tag set. use an
        // LRU or something.
        let score = rt
            .score(source_tags, target_tags, edge_tags, &global_lookup)
            .expect("error while computing score");

        score.penalty + (edge.distance as f32 * score.cost_factor)
    }

    pub fn find_route(&self, rt: &Runtime, from: Point, to: Point) -> Option<RouteResponse> {
        let from = self.snap_to_node(from)?;
        let to = self.snap_to_node(to)?;

        self.find_route_from_nodes(rt, from, to)
    }

    fn snap_to_node(&self, pt: Point) -> Option<NodeIndex> {
        let snap = self.index.snap_point(&self.inner, pt, 100.0)?;

        match snap {
            SnappedTo::Node(node_id) => Some(node_id),
            // TODO: properly handle this, need to inject the fake
            // edges to the graph.
            SnappedTo::Edge { node_id, .. } => Some(node_id),
        }
    }

    fn find_route_from_nodes(
        &self,
        rt: &Runtime,
        from: NodeIndex,
        to: NodeIndex,
    ) -> Option<RouteResponse> {
        let dest_node = self.node_data(to);
        let ruler = Ruler::for_point(&dest_node.point);

        let (cost, path) = astar(
            &self.inner,
            from,
            |node_id| node_id == to,
            |edge_ref| self.score_edge(rt, edge_ref),
            |node_id| ruler.dist_cheap(&self.node_data(node_id).point, &dest_node.point),
        )?;

        Some(self.build_route_response(cost, &path))
    }

    fn build_route_response(&self, cost: f32, node_ids: &[NodeIndex]) -> RouteResponse {
        let mut dist_meters = 0;
        let mut geometry = Vec::with_capacity(node_ids.len());

        for (i, &node_id) in node_ids[1..].iter().enumerate() {
            let prev_node_id = node_ids[i];

            // TODO: How do we figure out which edge was taken in the case of parallel ones?
            // There's another function: edges_connecting, which might be relevant.
            let (edge_idx, direction) = self
                .inner
                .find_edge_undirected(prev_node_id, node_id)
                .expect("no edge between given nodes");

            let edge = self.edge_data(edge_idx);
            dist_meters += edge.distance;

            match direction {
                Incoming => geometry.extend(&edge.points),
                Outgoing => geometry.extend(edge.points.iter().rev()),
            }
        }

        RouteResponse {
            cost,
            dist_meters,
            geometry,
            // surfaces: todo!(),
        }
    }
}

// enum SimpleSurface {
//     Asphalt,
//     Unpaved,
//     Cobblestone,
// }

// struct Interval<T> {
//     begin_index: usize,
//     end_index: usize,
//     value: T,
// }

pub struct RouteResponse {
    pub dist_meters: u32,
    pub cost: f32,
    pub geometry: Vec<Point>,
    // surfaces: Vec<Interval<SimpleSurface>>,
    // TODO: highway kind, elevation
}
