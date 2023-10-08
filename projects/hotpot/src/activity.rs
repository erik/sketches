use std::collections::HashMap;
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::Path;

use anyhow::Result;
use fitparser::de::{from_reader_with_options, DecodeOption};
use fitparser::profile::MesgNum;
use fitparser::Value;
use flate2::read::GzDecoder;
use geo::{EuclideanDistance, HaversineLength};
use geo_types::{LineString, MultiLineString, Point};
use rusqlite::params;
use time::OffsetDateTime;

use crate::db::{encode_line, SqlDateTime};
use crate::simplify::simplify_line;
use crate::tile::{BBox, LngLat, Tile, WebMercator};
use crate::{DEFAULT_TILE_EXTENT, DEFAULT_ZOOM_LEVELS};

// TODO: not happy with the ergonomics of this.
struct TileClipper {
    zoom: u8,
    current: Option<(Tile, BBox)>,
    tiles: HashMap<Tile, Vec<LineString<u16>>>,
}

impl TileClipper {
    fn new(zoom: u8) -> Self {
        Self {
            zoom,
            tiles: HashMap::new(),
            current: None,
        }
    }

    fn bounding_tile(&self, pt: &WebMercator) -> (Tile, BBox) {
        let tile = pt.tile(self.zoom);
        let bbox = tile.xy_bounds();
        (tile, bbox)
    }

    fn last_line(&mut self, tile: &Tile) -> &mut LineString<u16> {
        let lines = self.tiles.entry(*tile).or_insert_with(Vec::new);

        if lines.is_empty() {
            lines.push(LineString::new(vec![]));
        }

        lines.last_mut().unwrap()
    }

    fn add_line_segment(&mut self, start: WebMercator, end: WebMercator) {
        let (tile, bbox) = match self.current {
            Some(pair) => pair,
            None => {
                let pair = self.bounding_tile(&start);
                self.current = Some(pair);
                pair
            }
        };

        match bbox.clip_line(&start, &end) {
            // [start, end] doesn't intersect with the current tile at all, reposition it.
            None => {
                self.finish_segment();
                self.current = Some(self.bounding_tile(&start));
                // todo: should we add new segment after shifting bbox?
                // self.add_line_segment(start, end, c+1);
            }

            // [start, end] is at least partially contained within the current tile.
            Some((a, b)) => {
                let line = self.last_line(&tile);
                if line.0.is_empty() {
                    line.0
                        .push(a.to_pixel(&bbox, DEFAULT_TILE_EXTENT as u16).into());
                }

                line.0
                    .push(b.to_pixel(&bbox, DEFAULT_TILE_EXTENT as u16).into());

                // If we've modified the end point, we've left the current tile.
                if b != end {
                    self.finish_segment();

                    let (next_tile, next_bbox) = self.bounding_tile(&end);
                    if next_tile != tile {
                        self.current = Some((next_tile, next_bbox));
                        self.add_line_segment(b, end);
                    }
                }
            }
        }
    }

    fn finish_segment(&mut self) {
        if let Some((tile, _)) = self.current {
            self.tiles.entry(tile).and_modify(|lines| {
                lines.push(LineString::new(vec![]));
            });
        }
    }
}

pub struct ClippedTiles(Vec<TileClipper>);

impl ClippedTiles {
    pub fn iter(&self) -> impl Iterator<Item = (&Tile, &LineString<u16>)> {
        self.0
            .iter()
            .flat_map(|clip| clip.tiles.iter())
            .filter(|(_, lines)| !lines.is_empty())
            .flat_map(|(tile, lines)| lines.iter().map(move |line| (tile, line)))
    }
}

#[derive(Clone)]
pub struct RawActivity {
    pub title: Option<String>,
    pub start_time: Option<SqlDateTime>,
    pub duration_secs: Option<u64>,
    pub tracks: MultiLineString,
}

impl RawActivity {
    /// How far apart two points can be before we consider them to be
    /// a separate line segment.
    const MAX_POINT_DISTANCE: f64 = 5000.0;

    pub fn length(&self) -> f64 {
        self.tracks.iter().map(LineString::haversine_length).sum()
    }

    pub fn clip_to_tiles(&self, zooms: &[u8], trim_dist: f64) -> ClippedTiles {
        let mut clippers: Vec<_> = zooms.iter().map(|zoom| TileClipper::new(*zoom)).collect();

        for line in self.tracks.iter() {
            let points: Vec<_> = line
                .points()
                .map(LngLat::from)
                .filter_map(|pt| pt.xy())
                .collect();

            if points.len() < 2 {
                continue;
            }

            let first = &points[0].0;
            let last = &points[points.len() - 1].0;

            // Find points which are >= trim_dist away from start/end
            let start_idx = points
                .iter()
                .enumerate()
                .find(|(_, pt)| pt.0.euclidean_distance(first) >= trim_dist)
                .map(|(i, _)| i);

            let end_idx = points
                .iter()
                .rev()
                .enumerate()
                .find(|(_, pt)| pt.0.euclidean_distance(last) >= trim_dist)
                .map(|(i, _)| points.len() - 1 - i);

            if let Some((i, j)) = start_idx.zip(end_idx) {
                if i >= j {
                    continue;
                }

                let mut pairs = points[i..j].windows(2);
                while let Some(&[p0, p1]) = pairs.next() {
                    // Skip over large jumps
                    let len = p0.0.euclidean_distance(&p1.0);
                    if len > Self::MAX_POINT_DISTANCE {
                        continue;
                    }

                    for clip in clippers.iter_mut() {
                        clip.add_line_segment(p0, p1);
                    }
                }

                for clip in clippers.iter_mut() {
                    clip.finish_segment();
                }
            }
        }

        ClippedTiles(clippers)
    }
}

pub enum FileType {
    Gpx,
    Fit,
    Tcx,
}

pub enum CompressionType {
    None,
    Gzip,
}

pub fn read<R>(rdr: R, kind: FileType, comp: CompressionType) -> Result<Option<RawActivity>>
where
    R: Read + 'static,
{
    let mut reader: BufReader<Box<dyn Read>> = BufReader::new(match comp {
        CompressionType::None => Box::new(rdr),
        CompressionType::Gzip => Box::new(GzDecoder::new(rdr)),
    });

    match kind {
        FileType::Gpx => parse_gpx(&mut reader),
        FileType::Fit => parse_fit(&mut reader),
        // TODO: implement TCX
        FileType::Tcx => Ok(None),
    }
}

// TODO: should return a Result
pub fn read_file(p: &Path) -> Option<RawActivity> {
    let file_name = p.file_name()?;
    match get_file_type(file_name.to_str()?) {
        Some((file_type, comp)) => {
            let file = File::open(p).expect("open file");
            match read(file, file_type, comp) {
                Ok(activity) => activity,
                Err(e) => {
                    println!("Error reading {:?}: {:?}", p, e);
                    None
                }
            }
        }

        _ => None,
    }
}

fn parse_fit<R: Read>(r: &mut R) -> Result<Option<RawActivity>> {
    const SCALE_FACTOR: f64 = (1u64 << 32) as f64 / 360.0;

    let opts = [
        DecodeOption::SkipDataCrcValidation,
        DecodeOption::SkipHeaderCrcValidation,
    ]
    .into();

    let (mut start_time, mut duration_secs) = (None, None);
    let mut points = vec![];
    for data in from_reader_with_options(r, &opts)? {
        match data.kind() {
            MesgNum::FileId => {
                for f in data.fields() {
                    // Skip over virtual rides
                    // TODO: not an exhaustive check
                    if f.name() == "manufacturer" {
                        match f.value() {
                            Value::String(val) if val.as_str() == "zwift" => return Ok(None),
                            _ => {}
                        }
                    }
                }
            }
            MesgNum::Record => {
                let mut lat: Option<i64> = None;
                let mut lng: Option<i64> = None;

                for f in data.fields() {
                    match f.name() {
                        "position_lat" => lat = f.value().try_into().ok(),
                        "position_long" => lng = f.value().try_into().ok(),
                        "timestamp" => {
                            let ts: i64 = f.value().try_into()?;

                            match start_time {
                                None => start_time = Some(ts),
                                Some(t) => duration_secs = Some((ts - t) as u64),
                            }
                        }
                        _ => {}
                    }
                }

                if let (Some(lat), Some(lng)) = (lat, lng) {
                    let pt = Point::new(lng as f64, lat as f64) / SCALE_FACTOR;
                    points.push(pt);
                }
            }
            _ => {}
        }
    }

    if points.is_empty() {
        return Ok(None);
    }

    let line = points.into_iter().collect::<LineString>();
    Ok(Some(RawActivity {
        duration_secs,
        title: None,
        start_time: start_time
            .map(|ts| OffsetDateTime::from_unix_timestamp(ts).unwrap())
            .map(SqlDateTime),
        tracks: MultiLineString::from(line),
    }))
}

fn parse_gpx<R: Read>(reader: &mut R) -> Result<Option<RawActivity>> {
    let gpx = gpx::read(reader)?;

    // Just take the first track (generally the only one).
    let Some(track) = gpx.tracks.first() else {
        return Ok(None);
    };

    let start_time = gpx.metadata.and_then(|m| m.time).map(OffsetDateTime::from);

    // Grab the timestamp from the last point to calculate duration
    let end_time = track
        .segments
        .last()
        .and_then(|seg| seg.points.last())
        .and_then(|wpt| wpt.time)
        .map(|t| OffsetDateTime::from(t).unix_timestamp() as u64);

    let duration_secs = start_time
        .map(|t| t.unix_timestamp() as u64)
        .zip(end_time)
        .filter(|(start, end)| end > start)
        .map(|(start, end)| end - start);

    Ok(Some(RawActivity {
        duration_secs,
        start_time: start_time.map(SqlDateTime),
        title: track.name.clone(),
        tracks: track.multilinestring(),
    }))
}

/// Allows us to treat `bar.gpx.gz` the same as `bar.gpx`.
pub fn get_file_type(file_name: &str) -> Option<(FileType, CompressionType)> {
    let mut exts = file_name.rsplit('.');

    let (comp, ext) = match exts.next()? {
        "gz" => (CompressionType::Gzip, exts.next()?),
        ext => (CompressionType::None, ext),
    };

    match ext {
        "gpx" => Some((FileType::Gpx, comp)),
        "fit" => Some((FileType::Fit, comp)),
        "tcx" => Some((FileType::Tcx, comp)),
        _ => None,
    }
}

pub fn upsert(
    conn: &mut rusqlite::Connection,
    name: &str,
    activity: &RawActivity,
    trim_dist: f64,
) -> Result<i64> {
    let mut insert_coords = conn.prepare_cached(
        "\
        INSERT INTO activity_tiles (activity_id, z, x, y, coords) \
        VALUES (?, ?, ?, ?, ?)",
    )?;

    let num_rows = conn.execute(
        "\
        INSERT OR REPLACE \
        INTO activities (file, title, start_time, duration_secs, dist_meters)\
        VALUES (?, ?, ?, ?, ?)",
        params![
            name,
            activity.title,
            activity.start_time,
            activity.duration_secs,
            activity.length(),
        ],
    )?;

    let activity_id = conn.last_insert_rowid();

    // If we've affected more than one row, we've replaced an existing one... so we need to
    // delete the existing tiles.
    if num_rows != 1 {
        conn.execute(
            "DELETE FROM activity_tiles WHERE activity_id = ?",
            params![activity_id],
        )?;
    }

    let tiles = activity.clip_to_tiles(&DEFAULT_ZOOM_LEVELS, trim_dist);
    for (tile, line) in tiles.iter() {
        let coords = encode_line(&simplify_line(&line.0, 4.0))?;
        insert_coords.insert(params![activity_id, tile.z, tile.x, tile.y, coords])?;
    }

    Ok(activity_id)
}