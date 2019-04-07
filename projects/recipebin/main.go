package main

import (
	"crypto/hmac"
	"crypto/sha256"
	"database/sql"
	"log"
	"net/http"
	"net/url"
	"strings"
	"time"

	_ "github.com/mattn/go-sqlite3"
	_ "golang.org/x/crypto/bcrypt"
)

func main() {
	db := NewDB(":memory:")
	service := &Service{db: db}

	service.Serve()
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

CREATE UNIQUE INDEX IF NOT EXISTS unique_tags_by_recipe_id
  ON recipe_tags(tag, recipe_id);
`
)

func (db DB) initializeSchema() error {
	log.Printf("[db] applying base schema")
	_, err := db.conn.Exec(baseSchema)
	return err
}

type Service struct {
	db           *DB
	cookieSecret []byte
}

const (
	authCookieName = "u"
)

func (s *Service) signCookie(value string) string {
	mac := hmac.New(sha256.New, s.cookieSecret)
	mac.Write([]byte(value))

	return string(mac.Sum(nil))
}

func (s *Service) verifyCookie(c *http.Cookie) bool {
	// Check for expired cookies first
	if time.Now().After(c.Expires) {
		return false
	}

	// Next try to verify
	values := strings.SplitN(c.Value, ".", 2)
	if len(values) != 2 {
		return false
	}

	given := []byte(values[0])
	expected := []byte(s.signCookie(values[1]))

	return hmac.Equal(expected, given)
}

func (s *Service) requireLogin(h http.HandlerFunc) http.HandlerFunc {
	wrapper := func(w http.ResponseWriter, req *http.Request) {
		if cookie, err := req.Cookie(authCookieName); err != nil {
			goto unauthenticated
		} else if !s.verifyCookie(cookie) {
			goto unauthenticated
		}

		h(w, req)
		return

	unauthenticated:
		w.WriteHeader(http.StatusUnauthorized)
		w.Write([]byte("sign in first."))
	}

	return wrapper
}

type MiniMux struct {
	*http.ServeMux
}

func (m *MiniMux) HandleScoped(path string, h http.HandlerFunc) {
	m.HandleFunc(path, func(w http.ResponseWriter, req *http.Request) {
		p := strings.TrimPrefix(req.URL.Path, path)
		r2 := new(http.Request)
		*r2 = *req
		r2.URL = new(url.URL)
		*r2.URL = *req.URL
		r2.URL.Path = p
	})
}

func (s *Service) Serve() {
	mux := MiniMux{http.NewServeMux()}

	mux.HandleScoped("/user", s.handleUserResource)
	mux.HandleScoped("/session/", s.handleSessionResource)
	mux.HandleScoped("/recipe/", s.handleRecipeResource)
	mux.HandleScoped("/tag/", s.handleTagResource)

	mux.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		if req.URL.Path != "/" {
			http.NotFound(w, req)
		} else {
			s.handleListing(w, req)
		}
	})

	log.Printf("[service] starting http server")
	http.ListenAndServe(":8080", mux)
}

// POST /user
// PUT  /user
func (s *Service) handleUserResource(w http.ResponseWriter, req *http.Request) {

}

// POST /session/login
// GET  /session/logout
func (s *Service) handleSessionResource(w http.ResponseWriter, req *http.Request) {

}

// GET    /recipe
// POST   /recipe
// GET    /recipe/:id
// PUT    /recipe/:id
// DELETE /recipe/:id
func (s *Service) handleRecipeResource(w http.ResponseWriter, req *http.Request) {

}

// GET /tag
// GET /tag/:id
func (s *Service) handleTagResource(w http.ResponseWriter, req *http.Request) {

}

// GET /
func (s *Service) handleListing(w http.ResponseWriter, req *http.Request) {

}
