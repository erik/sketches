use std::convert::From;

use osmpbfreader::objects::Node as OsmNode;
use petgraph::visit::EdgeRef;
use petgraph::Direction::{Incoming, Outgoing};
use petgraph::{
    algo::astar,
    graph::NodeIndex,
    graph::{EdgeReference, Graph},
    Undirected,
};
use smartstring::{Compact, SmartString};

use crate::index::{Point2D, SpatialIndex};
use crate::profile::Runtime;
use crate::tags::{CompactTags, TagDict};

pub mod osm;

/// In radians
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LatLng {
    pub lat: f32,
    pub lon: f32,
}

impl LatLng {
    // Haversine, returns meters
    // TODO: unchecked
    fn dist_to(&self, other: &Self) -> f32 {
        let dt_lon = self.lon - other.lon;
        let dt_lat = self.lat - other.lat;

        let a = (dt_lat / 2.0_f32).sin();
        let b = (dt_lon / 2.0_f32).sin();
        let c = self.lat.cos() * other.lat.cos();
        let d = (a * a) + ((b * b) * c);
        let e = d.sqrt().asin();

        2_f32 * 6_372_800_f32 * e
    }
}

impl From<&OsmNode> for LatLng {
    fn from(node: &OsmNode) -> LatLng {
        let (lat, lon) = (node.lat() as f32, node.lon() as f32);

        LatLng {
            lat: lat.to_radians(),
            lon: lon.to_radians(),
        }
    }
}

impl From<LatLng> for Point2D {
    fn from(pt: LatLng) -> Point2D {
        (pt.lat, pt.lon)
    }
}

#[derive(Debug)]
pub struct NodeData {
    point: LatLng,
    tags: CompactTags,
}

#[derive(Debug, Clone)]
pub struct EdgeData {
    // meters
    // TODO: Can likely store this as dist / 10 and fit in u16 (max: 655km)
    dist: u32,
    tags: CompactTags,
    // TODO: Can delta-encode coordinates against start point to fit in (u16, u16)
    // TODO: Alternatively - polyline, without ASCII representation
    geometry: Vec<LatLng>,
}

pub struct OsmGraph {
    // TODO: use Csr, but petgraph doesn't support parallel edges, which we need.
    // TODO: Use a directed graph so we can represent one ways etc.
    pub inner: Graph<NodeData, EdgeData, Undirected>,
    pub index: SpatialIndex<NodeIndex, Point2D>,
    pub tag_dict: TagDict<SmartString<Compact>>,
    // TODO: This doesn't belong here
    pub runtime: Runtime,
}

impl OsmGraph {
    // TODO: edge score needs to include nodes
    fn score_edge(&self, edge_ref: EdgeReference<'_, EdgeData>) -> f32 {
        let edge = edge_ref.weight();
        let source_node = self
            .inner
            .node_weight(edge_ref.source())
            .expect("edge source node not in graph");
        let target_node = self
            .inner
            .node_weight(edge_ref.target())
            .expect("edge target node not in graph");

        let score = self
            .runtime
            .score(&source_node.tags, &target_node.tags, &edge.tags)
            .expect("error while computing score");

        score.penalty + (edge.dist as f32 * score.cost_factor)
    }

    pub fn find_route(&self, from: LatLng, to: LatLng) -> Option<Vec<LatLng>> {
        let from = self.index.find_nearest_within(from.into(), 500.0)?;
        let to = self.index.find_nearest_within(to.into(), 500.0)?;

        self.find_route_from_nodes(from, to)
    }

    fn find_route_from_nodes(&self, from: NodeIndex, to: NodeIndex) -> Option<Vec<LatLng>> {
        let dest_node = self.inner.node_weight(to).expect("Invalid `to` given.");

        let (cost, path) = astar(
            &self.inner,
            from,
            |node_id| node_id == to,
            |e| self.score_edge(e),
            |node_id| {
                self.inner
                    .node_weight(node_id)
                    .map(|n| n.point.dist_to(&dest_node.point))
                    .unwrap_or(0.0)
            },
        )?;

        println!("Total Cost = {:?} km (equiv)", cost as f32 / 1000.0);

        let path_geom = self.build_geometry(&path);
        Some(path_geom)
    }

    fn build_geometry(&self, node_ids: &[NodeIndex]) -> Vec<LatLng> {
        let mut geometry = Vec::with_capacity(node_ids.len());

        for (i, &node_id) in node_ids[1..].iter().enumerate() {
            let prev_node_id = node_ids[i];

            // TODO: How do we figure out which edge was taken in the case of parallel ones?
            // There's another function: edges_connecting, which might be relevant.
            self.inner
                .find_edge_undirected(prev_node_id, node_id)
                .and_then(|(edge_idx, direction)| {
                    let edge = self.inner.edge_weight(edge_idx)?;

                    match direction {
                        Incoming => geometry.extend(&edge.geometry),
                        Outgoing => geometry.extend(edge.geometry.iter().rev()),
                    };

                    Some(())
                })
                .expect("no edge between given nodes");
        }

        geometry
    }
}
