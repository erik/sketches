package main

import (
	"database/sql"
	"log"
	"net/http"

	_ "github.com/mattn/go-sqlite3"
)

func main() {
	db := NewDB(":memory:")
	s := &Service{db: db}

	s.Serve()
}

type DB struct {
	conn *sql.DB
}

func NewDB(uri string) *DB {
	conn, err := sql.Open("sqlite3", uri)
	if err != nil {
		panic(err)
	}

	db := DB{conn: conn}
	if err := db.initializeSchema(); err != nil {
		panic(err)
	}

	return &db
}

const (
	baseSchema = `
CREATE TABLE IF NOT EXISTS users(
  id INTEGER PRIMARY KEY autoincrement,

  name  TEXT,
  email TEXT,

  password TEXT,

  created_at DATETIME DEFAULT current_timestamp,
  updated_at DATETIME,
  deleted_at DATETIME
);

CREATE TABLE IF NOT EXISTS recipes(
  id INTEGER PRIMARY KEY autoincrement,

  title       TEXT,
  description TEXT,
  category    TEXT,

  instructions TEXT,
  ingredients  TEXT,
  rating       INT,

  created_at DATETIME DEFAULT current_timestamp,
  updated_at DATETIME,
  deleted_at DATETIME
);

CREATE TABLE IF NOT EXISTS recipe_tags(
  id INTEGER PRIMARY KEY autoincrement,

  recipe_id INTEGER,
  tag TEXT,

  FOREIGN KEY (recipe_id) REFERENCES recipes(id)
);

CREATE UNIQUE INDEX IF NOT EXISTS unique_tags_by_recipe_id ON recipe_tags(tag, recipe_id);
`
)

func (db DB) initializeSchema() error {
	log.Printf("[db] applying base schema")
	_, err := db.conn.Exec(baseSchema)
	return err
}

type Service struct {
	db *DB
}

func (s *Service) Serve() {
	log.Printf("[service] starting http server")
	http.ListenAndServe(":8080", s)
}

func (s *Service) ServeHTTP(w http.ResponseWriter, r *http.Request) {
}
