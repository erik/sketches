use std::convert::From;
use std::fs::File;
use std::hash::Hash;
use std::io::{BufRead, BufReader, ErrorKind};
use std::path::Path;
use std::{collections::HashMap, io::Error};

use osmpbfreader::Tags as OsmTags;
use smartstring::{Compact, SmartString};

pub const UNKNOWN_TAG_ID: TagDictId = 0;
pub type TagDictId = u16;

pub struct TagDict<S> {
    max_id: TagDictId,
    // TODO: Wasteful to store two copies of the key, maybe Rc<S> it
    forward: HashMap<S, TagDictId>,
    backward: HashMap<TagDictId, S>,
}

impl<S: Eq + Hash + Clone> TagDict<S> {
    fn with_unknown(unknown_value: S) -> Self {
        TagDict {
            max_id: UNKNOWN_TAG_ID,
            forward: HashMap::from([(unknown_value.clone(), UNKNOWN_TAG_ID)]),
            backward: HashMap::from([(UNKNOWN_TAG_ID, unknown_value)]),
        }
    }

    fn insert(&mut self, key: S) -> TagDictId {
        match self.forward.get(&key) {
            Some(&existing) => existing,
            None => {
                self.max_id += 1;
                self.forward.insert(key.clone(), self.max_id);
                self.backward.insert(self.max_id, key);
                self.max_id
            }
        }
    }

    fn to_compact(&self, key: &S) -> Option<TagDictId> {
        self.forward.get(key).copied()
    }

    fn from_compact(&self, key: &TagDictId) -> Option<&S> {
        self.backward.get(key)
    }
}

const IGNORED_KEY_PREFIXES: &[&str] = &["addr:", "name:", "source:", "tiger:"];
// TODO: lazy_static! HashSet might be faster as this grows.
// roughly sorted by usage.
const IGNORED_KEYS: &[&str] = &[
    "name",
    "source",
    "ref",
    "addr",
    "comment",
    "created_by",
    "fixme",
    "note",
];

fn ignore_osm_tag(key: &SmartString<Compact>, _val: &SmartString<Compact>) -> bool {
    IGNORED_KEYS.iter().any(|k| key == k)
        || IGNORED_KEY_PREFIXES
            .iter()
            .any(|prefix| key.starts_with(prefix))
}

impl TagDict<SmartString<Compact>> {
    pub fn new() -> Self {
        TagDict::with_unknown("unknown".into())
    }

    // TODO: serialization
    fn load(path: &Path) -> Result<Self, Error> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);

        let mut dict = TagDict::with_unknown("unknown".into());

        // TODO: size header to avoid allocations
        // TODO: escape separator chars

        for line in reader.lines() {
            let line = line?;
            let (key, vals) = line
                .split_once(' ')
                .ok_or_else(|| Error::new(ErrorKind::Other, "bad format"))?;

            dict.insert(key.into());
            for val in vals.split(';') {
                dict.insert(val.into());
            }
        }

        Ok(dict)
    }

    pub fn from_osm(&mut self, osm_tags: &OsmTags) -> CompactTags {
        let mut keys = Vec::with_capacity(osm_tags.len());

        for (k, v) in osm_tags.iter() {
            // FIXME: this is a dumb hack. OSM reader crate is using
            // an older version of smartstring.
            let (k, v) = (k.as_str().into(), v.as_str().into());

            if ignore_osm_tag(&k, &v) {
                continue;
            }

            // TODO: Don't insert here, use pre-built tag set to pare it down.
            keys.push(CompactTag {
                key: self.insert(k),
                val: self.insert(v),
            });
        }

        // We need to store the keys in ascending order so that our
        // binary search during lookup works.
        keys.sort_by_cached_key(|tag| tag.key);

        CompactTags { keys }
    }

    pub fn tag_source<'a>(&'a self, tags: &'a CompactTags) -> CompactTagSource {
        CompactTagSource { dict: self, tags }
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
    pub fn get_key<'a, S: Eq + Hash + Clone>(
        &self,
        dict: &'a TagDict<S>,
        key: &S,
    ) -> Option<&'a S> {
        let compact_key = dict.to_compact(key)?;
        let val = self.get_compact_key(compact_key)?;

        dict.from_compact(&val)
            .or_else(|| dict.from_compact(&UNKNOWN_TAG_ID))
    }

    fn get_compact_key(&self, key: TagDictId) -> Option<TagDictId> {
        self.keys
            .binary_search_by_key(&key, |tag| tag.key)
            .map(|idx| self.keys[idx].val)
            .ok()
    }
}

pub trait TagSource {
    fn get_tag(&self, k: &str) -> Option<&str>;
}

pub struct EmptyTagSource;
impl TagSource for EmptyTagSource {
    fn get_tag(&self, _k: &str) -> Option<&str> {
        None
    }
}

pub struct CompactTagSource<'a> {
    dict: &'a TagDict<SmartString<Compact>>,
    tags: &'a CompactTags,
}

impl<'a> TagSource for CompactTagSource<'a> {
    fn get_tag(&self, key: &str) -> Option<&str> {
        self.tags
            .get_key(self.dict, &key.into())
            .map(|s| s.as_str())
    }
}
