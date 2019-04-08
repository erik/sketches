package main

import (
	"crypto/hmac"
	"crypto/sha256"
	"database/sql"
	"fmt"
	"log"
	"net/http"
	"net/url"
	"strconv"
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

type UserModel struct {
	Id    int
	Name  string
	Email string

	Password string

	CreatedAt time.Time
	UpdatedAt time.Time
}

// todo
type RecipeModel struct{}

type RecipeTagModel struct {
	Id       int
	RecipeId int
	Tag      string
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
  updated_at DATETIME
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
  updated_at DATETIME
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

func (db DB) UserById(id int) (*UserModel, error) {
	// TODO
	return nil, nil
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

// returns (user_id, verified?)
func (s *Service) verifyCookie(c *http.Cookie) (int, bool) {
	// Check for expired cookies first
	if time.Now().After(c.Expires) {
		return 0, false
	}

	// Next try to verify
	values := strings.SplitN(c.Value, ".", 2)
	if len(values) != 2 {
		return 0, false
	}

	given := []byte(values[0])
	expected := []byte(s.signCookie(values[1]))

	if hmac.Equal(expected, given) {
		uid, _ := strconv.Atoi(values[1])
		return uid, true
	}

	return 0, false
}

func (s *Service) loggedInUser(req *http.Request) *UserModel {
	if cookie, err := req.Cookie(authCookieName); err != nil {
		return nil
	} else if uid, ok := s.verifyCookie(cookie); !ok {
		return nil
	} else if user, err := s.db.UserById(uid); err != nil {
		log.Printf("user lookup failed: %+v\n", err)
		return nil
	} else {
		return user
	}
}

type MiniMux struct {
	*http.ServeMux
}

func (m *MiniMux) HandleScoped(path string, h http.HandlerFunc) {
	m.HandleFunc(path, func(w http.ResponseWriter, req *http.Request) {
		// Implementation stolen from `http.StripPrefix` (which takes a
		// http.Handler instead)
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

func sendStatus(w http.ResponseWriter, statusCode int) {
	w.WriteHeader(statusCode)
	w.Write([]byte(http.StatusText(statusCode)))
}

// POST /user
// PUT  /user
func (s *Service) handleUserResource(w http.ResponseWriter, req *http.Request) {
	email := req.FormValue("email")
	password := req.FormValue("password")

	switch req.Method {
	case http.MethodPost:
		fmt.Printf("create user email=%s password=%s\n", email, password)

	case http.MethodPut:
		user := s.loggedInUser(req)
		if user == nil {
			sendStatus(w, http.StatusUnauthorized)
			return
		}

		fmt.Printf("update user (%+v) email=%s password=%s\n",
			user, email, password)

	default:
		http.NotFound(w, req)
	}
}

// GET  /session/login
// POST /session/login
// GET  /session/logout
func (s *Service) handleSessionResource(w http.ResponseWriter, req *http.Request) {
	type createLoginBody struct {
		Email    string
		Password string
	}

	path := req.URL.Path

	switch {
	case req.Method == http.MethodGet && path == "login":
		// todo

	case req.Method == http.MethodPost && path == "login":
		// todo

		http.Redirect(w, req, "/", http.StatusSeeOther)

	case req.Method == http.MethodGet && path == "logout":
		http.SetCookie(w, &http.Cookie{
			Name:     authCookieName,
			Value:    "",
			Path:     "/",
			Expires:  time.Unix(0, 0),
			HttpOnly: true,
		})

		http.Redirect(w, req, "/", http.StatusSeeOther)

	default:
		http.NotFound(w, req)
	}

}

// GET    /recipe
// POST   /recipe
// GET    /recipe/:id
// PUT    /recipe/:id
// DELETE /recipe/:id
func (s *Service) handleRecipeResource(w http.ResponseWriter, req *http.Request) {
	type createRecipeBody struct{}
	type updateRecipeBody struct{}
}

// GET /tag
// GET /tag/:id
func (s *Service) handleTagResource(w http.ResponseWriter, req *http.Request) {

}

// GET /
func (s *Service) handleListing(w http.ResponseWriter, req *http.Request) {

}
