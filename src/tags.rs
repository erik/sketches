#![allow(dead_code)]

use std::convert::From;

use osmpbfreader::Tags as OsmTags;

#[derive(Clone, Debug)]
pub struct EdgeTags {
    pub highway: HighwayKind,
    pub surface: SurfaceKind,
    pub smoothness: SmoothnessKind,
    // TODO: access, one way, etc
    pub raw: OsmTags,
}

impl From<OsmTags> for EdgeTags {
    fn from(tags: OsmTags) -> EdgeTags {
        EdgeTags {
            highway: HighwayKind::from(&tags),
            surface: SurfaceKind::from(&tags),
            smoothness: SmoothnessKind::from(&tags),
            raw: tags,
        }
    }
}

#[derive(Clone, Debug)]
pub struct NodeTags {
    // TODO: Barrier, junction
    pub raw: OsmTags,
}

impl From<OsmTags> for NodeTags {
    fn from(tags: OsmTags) -> NodeTags {
        NodeTags { raw: tags }
    }
}

#[derive(Clone, Debug)]
pub enum SmoothnessKind {
    Excellent,
    Good,
    Intermediate,
    Bad,
    VeryBad,
    Horrible,
    Unknown,
}

impl From<&OsmTags> for SmoothnessKind {
    fn from(tags: &OsmTags) -> SmoothnessKind {
        use SmoothnessKind::*;

        match tags.get("smoothness").map(|v| v.as_str()).unwrap_or("") {
            "excellent" => Excellent,
            "good" => Good,
            "intermediate" => Intermediate,
            "bad" => Bad,
            "very_bad" => VeryBad,
            "horrible" => Horrible,
            _ => Unknown,
        }
    }
}

#[derive(Clone, Debug)]
pub enum HighwayKind {
    Bridleway,
    Cycleway,
    Footway,
    Motorway,
    MotorwayLink,
    Path,
    Primary,
    PrimaryLink,
    Residential,
    Secondary,
    SecondaryLink,
    Service,
    Steps,
    Tertiary,
    TertiaryLink,
    Track,
    Trunk,
    TrunkLink,
    Unclassified,
    Unknown,
}

impl From<&OsmTags> for HighwayKind {
    fn from(tags: &OsmTags) -> HighwayKind {
        use HighwayKind::*;

        match tags.get("highway").map(|v| v.as_str()).unwrap_or("") {
            "bridleway" => Bridleway,
            "cycleway" => Cycleway,
            "footway" => Footway,
            "motorway" => Motorway,
            "motorway_link" => MotorwayLink,
            "path" => Path,
            "primary" => Primary,
            "primary_link" => PrimaryLink,
            "residential" => Residential,
            "secondary" => Secondary,
            "secondary_link" => SecondaryLink,
            "service" => Service,
            "steps" => Steps,
            "tertiary" => Tertiary,
            "tertiary_link" => TertiaryLink,
            "track" => Track,
            "trunk" => Trunk,
            "trunk_link" => TrunkLink,
            "unclassified" => Unclassified,
            _ => Unknown,
        }
    }
}

// TODO: Narrow these down, too much granularity
#[derive(Clone, Debug)]
pub enum SurfaceKind {
    Paved(PavementKind),
    Unpaved(UnpavedKind),
    Cobblestone(CobblestoneKind),
    Unknown,
}

#[derive(Clone, Debug)]
pub enum PavementKind {
    Generic,
    Asphalt,
    Chipseal,
    Concrete,
    ConcreteLanes,
    ConcretePlates,
    Wood,
    Metal,
}

#[derive(Clone, Debug)]
pub enum CobblestoneKind {
    Generic,
    PavingStones,
    Sett,
    Unhewn,
}

#[derive(Clone, Debug)]
pub enum UnpavedKind {
    Generic,
    Compacted,
    FineGravel,
    Gravel,
    Rock,
    Mud,
    Sand,
    Woodchips,
    Snow,
}

impl From<&OsmTags> for SurfaceKind {
    fn from(tags: &OsmTags) -> SurfaceKind {
        use SurfaceKind::*;
        match tags.get("surface").map(|v| v.as_str()).unwrap_or("") {
            // paved
            "paved" => Paved(PavementKind::Generic),
            "asphalt" => Paved(PavementKind::Asphalt),
            "chipseal" => Paved(PavementKind::Chipseal),
            "concrete" => Paved(PavementKind::Concrete),
            "concrete:lanes" => Paved(PavementKind::ConcreteLanes),
            "concrete:plates" => Paved(PavementKind::ConcretePlates),
            "wood" => Paved(PavementKind::Wood),
            "metal" => Paved(PavementKind::Metal),

            // cobblestone
            "cobblestone" => Cobblestone(CobblestoneKind::Generic),
            "paving_stones" => Cobblestone(CobblestoneKind::PavingStones),
            "sett" => Cobblestone(CobblestoneKind::Sett),
            "unhewn_cobblestone" => Cobblestone(CobblestoneKind::Unhewn),

            // unpaved
            "unpaved" | "ground" => Unpaved(UnpavedKind::Generic),
            "compacted" | "pebblestone" | "dirt" | "earth" => Unpaved(UnpavedKind::Compacted),
            "fine_gravel" => Unpaved(UnpavedKind::FineGravel),
            "gravel" => Unpaved(UnpavedKind::Gravel),
            "rock" => Unpaved(UnpavedKind::Rock),
            "mud" => Unpaved(UnpavedKind::Mud),
            "sand" => Unpaved(UnpavedKind::Sand),
            "woodchips" => Unpaved(UnpavedKind::Woodchips),
            "snow" | "ice" => Unpaved(UnpavedKind::Snow),
            _ => Unknown,
        }
    }
}
