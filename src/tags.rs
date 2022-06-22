#![allow(dead_code)]

use std::convert::From;
use std::fs::File;
use std::hash::Hash;
use std::io::{BufRead, BufReader, ErrorKind};
use std::path::Path;
use std::{collections::HashMap, io::Error};

use osmpbfreader::Tags as OsmTags;
use smartstring::{Compact, SmartString};

pub type TagDictId = u16;

pub struct TagDict<S> {
    max_id: TagDictId,
    forward: HashMap<S, TagDictId>,
    backward: HashMap<TagDictId, S>,
}

impl<S: Eq + Hash + Clone> TagDict<S> {
    fn new() -> Self {
        TagDict {
            max_id: TagDictId::default(),
            forward: HashMap::new(),
            backward: HashMap::new(),
        }
    }

    fn insert(&mut self, key: S) -> TagDictId {
        match self.forward.get(&key) {
            Some(&id) => id,
            None => {
                self.forward.insert(key.clone(), self.max_id);
                self.backward.insert(self.max_id, key);
                self.max_id += 1;

                self.max_id
            }
        }
    }
}

impl<S: Eq + Hash> TagDict<S> {
    fn to_compact(&self, key: &S) -> Option<&TagDictId> {
        self.forward.get(&key)
    }

    fn from_compact(&self, key: &TagDictId) -> Option<&S> {
        self.backward.get(&key)
    }
}

impl TagDict<SmartString<Compact>> {
    // TODO: serialization
    fn load(path: &Path) -> Result<Self, Error> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);

        let mut dict = TagDict::new();

        // TODO: size header to avoid allocations
        // TODO: escape separator chars

        for line in reader.lines() {
            let line = line?;
            let (key, vals) = line
                .split_once(" ")
                .ok_or_else(|| Error::new(ErrorKind::Other, "bad format"))?;

            dict.insert(key.into());
            for val in vals.split(";") {
                dict.insert(val.into());
            }
        }

        Ok(dict)
    }
}

#[derive(Debug, Clone)]
pub struct CompactTag {
    key: TagDictId,
    val: TagDictId,
}

#[derive(Debug, Clone)]
pub struct CompactTags {
    keys: Vec<CompactTag>,
}

impl CompactTags {
    fn get_key<'a, S: Eq + Hash>(&self, dict: &'a TagDict<S>, key: &S) -> Option<&'a S> {
        let key = dict.to_compact(key)?;
        let val = self.get_compact_key(*key)?;

        dict.from_compact(&val)
    }

    fn get_compact_key(&self, key: TagDictId) -> Option<TagDictId> {
        self.keys
            .binary_search_by_key(&key, |tag| tag.key)
            .map(|idx| self.keys[idx].val)
            .ok()
    }
}

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
