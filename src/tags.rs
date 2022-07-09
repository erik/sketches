use std::collections::HashMap;
use std::convert::From;
use std::hash::Hash;

use osmpbfreader::Tags as OsmTags;
use smartstring::{Compact, SmartString};

pub const UNKNOWN_TAG_ID: TagDictId = 0;
pub type TagDictId = u16;
pub type CompactString = SmartString<Compact>;

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

    pub fn insert(&mut self, key: S) -> TagDictId {
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

    pub fn to_compact(&self, key: &S) -> Option<TagDictId> {
        self.forward.get(key).copied()
    }

    fn from_compact(&self, key: &TagDictId) -> Option<&S> {
        self.backward.get(key)
    }
}

const IGNORED_KEY_PREFIXES: &[&str] = &[
    "addr:",
    "name:",
    "source:",
    "network:",
    "operator:",
    "tiger:",
];
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
    "network",
    "operator",
];

fn ignore_osm_tag(key: &CompactString, _val: &CompactString) -> bool {
    IGNORED_KEYS.iter().any(|k| key == k)
        || IGNORED_KEY_PREFIXES
            .iter()
            .any(|prefix| key.starts_with(prefix))
}

impl TagDict<CompactString> {
    pub fn new() -> Self {
        TagDict::with_unknown("unknown".into())
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
    pub fn contains_key<S: Eq + Hash + Clone>(&self, dict: &TagDict<S>, key: &S) -> bool {
        dict.to_compact(key)
            .and_then(|k| self.get_compact_key(k))
            .is_some()
    }

    pub fn get_key<'a, S: Eq + Hash + Clone>(
        &self,
        dict: &'a TagDict<S>,
        key: &S,
    ) -> Option<&'a S> {
        let compact_key = dict.to_compact(key)?;
        let val = self.get_compact_key(compact_key)?;

        dict.from_compact(val)
            .or_else(|| dict.from_compact(&UNKNOWN_TAG_ID))
    }

    // TODO: only returns a ref for the interface - not needed
    fn get_compact_key(&self, key: TagDictId) -> Option<&TagDictId> {
        self.keys
            .binary_search_by_key(&key, |tag| tag.key)
            .map(|idx| &self.keys[idx].val)
            .ok()
    }
}

pub trait TagSource<K, V> {
    fn has_tag(&self, k: &K) -> bool;
    fn get_tag(&self, k: &K) -> Option<&V>;
}

pub struct EmptyTagSource;
impl<K, V> TagSource<K, V> for EmptyTagSource {
    fn has_tag(&self, _k: &K) -> bool {
        false
    }
    fn get_tag(&self, _k: &K) -> Option<&V> {
        None
    }
}

pub struct CompactTagSource<'a> {
    dict: &'a TagDict<CompactString>,
    tags: &'a CompactTags,
}

impl<'a> TagSource<CompactString, CompactString> for CompactTagSource<'a> {
    fn has_tag(&self, key: &CompactString) -> bool {
        self.tags.contains_key(self.dict, key)
    }
    fn get_tag(&self, key: &CompactString) -> Option<&CompactString> {
        self.tags.get_key(self.dict, key)
    }
}

impl TagSource<TagDictId, TagDictId> for CompactTags {
    fn has_tag(&self, key: &TagDictId) -> bool {
        self.get_compact_key(*key).is_some()
    }
    fn get_tag(&self, key: &TagDictId) -> Option<&TagDictId> {
        self.get_compact_key(*key)
    }
}
