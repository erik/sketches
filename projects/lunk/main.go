package main

import (
	"database/sql"
	"flag"
	"log"
	"net/http"

	_ "github.com/mattn/go-sqlite3"
	"golang.org/x/crypto/bcrypt"
)

const USAGE = `lunk - personal bookmark collection

Usage:
  lunk [-db <db_path>] [-http <bind_host>]

Options:
  -db   <db_path>    path to the Sqlite3 database to use [default: ./lunk.db]
  -http <bind_host>  addr to serve rendered schema on [default: ':8080']
`

// CLI flags
var (
	dbPath   string
	bindHost string
)

func init() {
	flag.StringVar(&dbPath, "db", "lunk.db", "path to database")
	flag.StringVar(&bindHost, "http", ":8080", "host to listen on")

	flag.Parse()
}

func main() {
	db, err := NewDB(dbPath)
	if err != nil {
		log.Fatal("unable to connect to db", err)
	}

	db.addUser("blah", "blah")

	web := NewWeb(db)
	web.Serve(bindHost)
}

type Web struct {
	db *DB
}

func NewWeb(db *DB) Web {
	return Web{
		db: db,
	}
}

func (w *Web) Serve(bindHost string) {
	mux := http.NewServeMux()

	// TODO: Sub in Gorilla?

	log.Printf("[web] starting up on %s", bindHost)
	http.ListenAndServe(bindHost, mux)
}

var MIGRATIONS = []string{
	`CREATE TABLE schema_versions (version INT, ts DATETIME DEFAULT current_timestamp)`,
	`
CREATE TABLE IF NOT EXISTS users (
  id        INTEGER PRIMARY KEY autoincrement,
  username  TEXT UNIQUE NOT NULL,
  pw_hash   TEXT UNIQUE NOT NULL
);

CREATE TABLE IF NOT EXISTS lunks (
  id          INTEGER PRIMARY KEY autoincrement,
  user_id     INT NOT NULL,

  url         TEXT NOT NULL,
  description TEXT NOT NULL,
  via         TEXT,

  created_at DATETIME DEFAULT current_timestamp,
  deleted_at DATETIME,

  FOREIGN KEY (user_id) REFERENCES users(id)
);

CREATE TABLE IF NOT EXISTS tags (
  id      INTEGER PRIMARY KEY autoincrement,
  lunk_id INTEGER,
  tag     TEXT,

  FOREIGN KEY (lunk_id) REFERENCES lunks(id)
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_uniq_tags ON tags(lunk_id, tag);
-- Does sqlite use the above index for this already?
CREATE        INDEX IF NOT EXISTS idx_tags_by_lunk_id ON tags(lunk_id);
`}

type DB struct {
	conn *sql.DB
}

func NewDB(uri string) (*DB, error) {
	conn, err := sql.Open("sqlite3", uri)
	if err != nil {
		return nil, err
	}

	db := DB{conn: conn}
	if err := db.initializeSchema(); err != nil {
		return nil, err
	}

	return &db, nil
}

func (db DB) initializeSchema() error {
	maxVersion := db.maxMigration()
	if maxVersion >= len(MIGRATIONS) {
		log.Printf("[db] schema up to date")
		return nil
	}

	for i := maxVersion + 1; i < len(MIGRATIONS); i++ {
		log.Printf("[db] applying migration %d...", i)
		if _, err := db.conn.Exec(MIGRATIONS[i]); err != nil {
			return err
		}
	}

	sql := `INSERT INTO schema_versions (version) VALUES (?)`
	_, err := db.conn.Exec(sql, len(MIGRATIONS))
	return err
}

func (db DB) maxMigration() int {
	version := -1
	sql := `SELECT COALESCE(MAX(version), -1) FROM schema_versions;`
	_ = db.conn.QueryRow(sql).Scan(&version)
	return version
}

func (db DB) addUser(name, pw string) error {
	if pwHash, err := hashPassword(pw); err != nil {
		return err
	} else {
		sql := `INSERT INTO users (username, pw_hash) VALUES (?,?)`
		_, err = db.conn.Exec(sql, name, pwHash)
		return err
	}
}

func (db DB) validateLogin(user, pw string) (bool, error) {
	if pwHash, err := hashPassword(pw); err != nil {
		return false, err
	} else {
		var res int
		sql := `SELECT EXISTS(SELECT 1 FROM users WHERE username = ? AND pw_hash = ?)`
		err = db.conn.QueryRow(sql, user, pwHash).Scan(&res)
		return (res == 1), err
	}
}

func hashPassword(pw string) (string, error) {
	bytes, err := bcrypt.GenerateFromPassword([]byte(pw), 14)
	return string(bytes), err
}
