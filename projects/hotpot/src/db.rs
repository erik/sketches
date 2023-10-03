use std::io::Cursor;
use std::path::Path;

use anyhow::Result;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use geo_types::Coord;
use r2d2_sqlite::SqliteConnectionManager;

use crate::{DEFAULT_TILE_EXTENT, DEFAULT_ZOOM_LEVELS};

const MIGRATIONS: [&str; 2] = [
    "-- Create migrations table
CREATE TABLE IF NOT EXISTS migrations (
    id         INTEGER PRIMARY KEY,
    created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);",
    "-- Initial schema
CREATE TABLE metadata (
      key   TEXT NOT NULL
    , value TEXT NOT NULL
);

CREATE TABLE activities (
      id            INTEGER PRIMARY KEY
    -- TODO: maybe do a hash of contents?
    , file          TEXT NOT NULL
    , title         TEXT
    , start_time    INTEGER
    , duration_secs INTEGER
    , dist_meters   REAL NOT NULL

    -- TODO:
    -- , kind     TEXT -- run, bike, etc
    -- , polyline TEXT
);

CREATE TABLE activity_tiles (
      id          INTEGER PRIMARY KEY
    , activity_id INTEGER NOT NULL
    , z           INTEGER NOT NULL
    , x           INTEGER NOT NULL
    , y           INTEGER NOT NULL
    , coords      BLOB NOT NULL
);

CREATE INDEX activity_tiles_activity_id ON activity_tiles (activity_id);
CREATE INDEX activity_tiles_zxy ON activity_tiles (z, x, y);",
];

pub struct Database {
    pool: r2d2::Pool<SqliteConnectionManager>,
    pub meta: Metadata,
}

impl Database {
    pub fn delete(path: &Path) -> Result<()> {
        let db_files = [path, &path.join("-wal"), &path.join("-shm")];

        println!("Removing existing DB.");
        for p in &db_files {
            match std::fs::remove_file(p) {
                Ok(_) => {
                    println!("\t{}", p.display());
                }
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
                Err(e) => panic!("error removing db: {}", e),
            }
        }

        Ok(())
    }

    pub fn new(path: &Path) -> Result<Self> {
        let manager = SqliteConnectionManager::file(path);
        let pool = r2d2::Pool::new(manager)?;
        let mut conn = pool.get()?;

        let pragmas = [("journal_mode", "WAL"), ("synchronous", "OFF")];
        for (k, v) in &pragmas {
            conn.pragma_update(None, k, v)?;
        }

        apply_migrations(&mut conn)?;

        // TODO: need to write metadata if it doesn't exist (and support updates)
        let meta = read_metadata(&mut conn)?;

        Ok(Database { pool, meta })
    }

    /// Open an existing database, fail if it doesn't exist
    pub fn open(path: &Path) -> Result<Self> {
        if !path.exists() {
            anyhow::bail!("database does not exist: {}", path.display());
        }

        Self::new(path)
    }

    pub fn connection(&self) -> Result<r2d2::PooledConnection<SqliteConnectionManager>> {
        self.pool.get().map_err(Into::into)
    }

    pub fn shared_pool(&self) -> r2d2::Pool<SqliteConnectionManager> {
        self.pool.clone()
    }
}

fn apply_migrations(conn: &mut rusqlite::Connection) -> Result<()> {
    let cur_migration: usize = conn
        .query_row("SELECT max(id) FROM migrations", [], |row| row.get(0))
        .unwrap_or(0);

    if cur_migration == MIGRATIONS.len() {
        return Ok(());
    }

    println!("Applying migrations");
    let tx = conn.transaction()?;
    for (i, m) in MIGRATIONS[cur_migration..].iter().enumerate() {
        println!("\t{}", cur_migration + i + 1);
        tx.execute_batch(m)?;
        tx.execute(
            "INSERT INTO migrations (id) VALUES (?)",
            [cur_migration + i + 1],
        )?;
    }
    tx.commit()?;
    println!("Done.");

    Ok(())
}

pub struct Metadata {
    pub zoom_levels: Vec<u8>,
    pub stored_width: u32,
}

impl Metadata {
    pub fn source_level(&self, target_zoom: u8) -> Option<u8> {
        for z in &self.zoom_levels {
            if *z >= target_zoom {
                return Some(*z);
            }
        }
        None
    }
}

impl Default for Metadata {
    fn default() -> Self {
        Metadata {
            zoom_levels: DEFAULT_ZOOM_LEVELS.to_vec(),
            stored_width: DEFAULT_TILE_EXTENT,
        }
    }
}

fn read_metadata(conn: &mut rusqlite::Connection) -> Result<Metadata> {
    let mut meta = Metadata::default();

    let mut stmt = conn.prepare("SELECT key, value FROM metadata")?;
    let mut rows = stmt.query([])?;

    while let Some(row) = rows.next()? {
        let key: String = row.get(0)?;
        match key.as_str() {
            "zoom_levels" => {
                meta.zoom_levels = row
                    .get_unwrap::<_, String>(1)
                    .split(',')
                    .map(|s| s.parse::<u8>().expect("zoom level"))
                    .collect();
            }

            "stored_width" => {
                meta.stored_width = row.get_unwrap(1);
            }

            unk => {
                println!("Ignoring unknown metadata key: {}", unk);
            }
        }
    }

    Ok(meta)
}

// TODO: consider piping this through a compression step.
pub fn encode_line(data: &[Coord<u16>]) -> Result<Vec<u8>> {
    let mut w = Vec::with_capacity(data.len() * 2);
    for pt in data {
        w.write_u16::<LittleEndian>(pt.x)?;
        w.write_u16::<LittleEndian>(pt.y)?;
    }
    Ok(w)
}

pub fn decode_line(bytes: &[u8]) -> Result<Vec<Coord<u32>>> {
    let mut coords = Vec::with_capacity(bytes.len() / 4);
    let mut reader = Cursor::new(bytes);
    while reader.position() < bytes.len() as u64 {
        let x = reader.read_u16::<LittleEndian>()?;
        let y = reader.read_u16::<LittleEndian>()?;
        coords.push(Coord {
            x: x as u32,
            y: y as u32,
        });
    }
    Ok(coords)
}