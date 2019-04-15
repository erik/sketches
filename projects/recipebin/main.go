package main

import (
	"crypto/hmac"
	"crypto/sha256"
	"database/sql"
	"encoding/hex"
	"fmt"
	"html/template"
	"log"
	"net/http"
	"net/url"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	_ "github.com/mattn/go-sqlite3"
	"golang.org/x/crypto/bcrypt"
)

func main() {
	// TODO: These strings must be configurable (with flag or sth)
	db := NewDB("/tmp/recipebin.db")
	directory := "templates"
	cookieSecret := ""

	service := NewService(db, cookieSecret, directory)
	service.Serve()
}

type DB struct {
	conn *sql.DB
}

type UserModel struct {
	Id    int
	Email string

	Password string

	CreatedAt time.Time
	UpdatedAt sql.NullInt64
}

type RecipeModel struct {
	Id           int
	UserId       int
	Title        string
	Description  sql.NullString
	Category     sql.NullString
	Notes        sql.NullString
	Instructions string
	Ingredients  string

	CreatedAt time.Time
	UpdatedAt sql.NullInt64
}

func (r RecipeModel) Validate() bool {
	v := false

	// TODO: remove magic consts
	switch {
	case r.UserId == 0:
	case r.Title == "" || len(r.Title) > 256:
	case r.Instructions == "" || len(r.Instructions) > 4096:
	case r.Ingredients == "" || len(r.Ingredients) > 4096:
	case len(r.Description.String) > 4096:
	case len(r.Category.String) > 4096:
	case len(r.Notes.String) > 4096:

	default:
		v = true
	}

	return v
}

type RecipeView struct {
	Id           int
	Title        string
	Description  string
	Category     string
	Notes        []string
	Instructions []string
	Ingredients  []string
}

func (r RecipeModel) View() RecipeView {
	return RecipeView{
		Id:           r.Id,
		Title:        r.Title,
		Description:  r.Description.String,
		Category:     r.Category.String,
		Notes:        strings.Split(r.Notes.String, "\n"),
		Instructions: strings.Split(r.Instructions, "\n"),
		Ingredients:  strings.Split(r.Ingredients, "\n"),
	}
}

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

  email TEXT UNIQUE NOT NULL,

  password TEXT UNIQUE NOT NULL,

  created_at DATETIME DEFAULT current_timestamp,
  updated_at DATETIME
);

CREATE TABLE IF NOT EXISTS recipes(
  id INTEGER PRIMARY KEY autoincrement,

  user_id INTEGER NOT NULL,

  title       TEXT NOT NULL,
  description TEXT,
  category    TEXT,
  notes       TEXT,

  instructions TEXT NOT NULL,
  ingredients  TEXT NOT NULL,

  created_at DATETIME DEFAULT current_timestamp,
  updated_at DATETIME
);

CREATE TABLE IF NOT EXISTS recipe_tags(
  id INTEGER PRIMARY KEY autoincrement,

  recipe_id INTEGER,
  tag TEXT,

  FOREIGN KEY (recipe_id) REFERENCES recipes(id)
);

CREATE UNIQUE INDEX IF NOT EXISTS unique_user_emails
  ON users(lower(email));

CREATE UNIQUE INDEX IF NOT EXISTS unique_tags_by_recipe_id
  ON recipe_tags(tag, recipe_id);
`
)

func (db DB) initializeSchema() error {
	log.Printf("[db] applying base schema")
	_, err := db.conn.Exec(baseSchema)
	return err
}

func (db DB) NewUser(email, password string) error {
	sql := `
INSERT INTO users (email, password)
VALUES (?, ?)
`

	if hash, err := HashPassword(password); err != nil {
		return err
	} else {
		_, err = db.conn.Exec(sql, email, hash)
		return err
	}
}

func (db DB) UserById(id int) (*UserModel, error) {
	var user UserModel

	err := db.conn.QueryRow(`
SELECT id
     , email
     , password
     , created_at
     , updated_at
FROM users
WHERE id = ?`, id).Scan(
		&user.Id,
		&user.Email,
		&user.Password,
		&user.CreatedAt,
		&user.UpdatedAt)

	return &user, err
}

func (db DB) UserByEmailAndPassword(email, password string) (*UserModel, error) {
	var user UserModel

	err := db.conn.QueryRow(`
SELECT id
     , email
     , password
     , created_at
     , updated_at
FROM users
WHERE lower(email) = lower(?)`, email).Scan(
		&user.Id,
		&user.Email,
		&user.Password,
		&user.CreatedAt,
		&user.UpdatedAt)

	if err != nil {
		return nil, err
	}

	err = bcrypt.CompareHashAndPassword(
		[]byte(user.Password),
		[]byte(password))
	return &user, err
}

func (db DB) NewRecipe(r RecipeModel) (int64, error) {
	sql := `
INSERT INTO recipes (
    user_id
  , title
  , description
  , category
  , notes
  , instructions
  , ingredients
)
VALUES (?, ?, ?, ?, ?, ?)`

	res, err := db.conn.Exec(sql,
		r.UserId,
		r.Title,
		r.Description,
		r.Category,
		r.Notes,
		r.Instructions,
		r.Ingredients,
	)

	if err != nil {
		return -1, err
	}

	return res.LastInsertId()
}

func (db DB) RecipeById(id int) (*RecipeModel, error) {
	var recipe RecipeModel

	err := db.conn.QueryRow(`
SELECT id
     , user_id
     , title
     , description
     , category
     , notes
     , instructions
     , ingredients
     , created_at
     , updated_at
FROM recipes
WHERE id = ?`, id).Scan(
		&recipe.Id,
		&recipe.UserId,
		&recipe.Title,
		&recipe.Description,
		&recipe.Category,
		&recipe.Notes,
		&recipe.Instructions,
		&recipe.Ingredients,
		&recipe.CreatedAt,
		&recipe.UpdatedAt)

	return &recipe, err
}

func HashPassword(password string) (string, error) {
	bytes, err := bcrypt.GenerateFromPassword([]byte(password), 14)
	return string(bytes), err
}

type Service struct {
	db           *DB
	cookieSecret []byte

	templates map[string]*template.Template
}

func NewService(db *DB, cookieSecret, tmplDir string) *Service {
	return &Service{
		db:           db,
		cookieSecret: []byte(cookieSecret),
		templates:    loadTemplates(tmplDir),
	}
}

func loadTemplates(dir string) map[string]*template.Template {
	// Normalize directory path to end in slash
	if dir != "" && dir[len(dir)-1] != '/' {
		dir = dir + "/"
	}

	// Everything starting with `_` is a helper template
	pattern := filepath.Join(dir, "_*.tmpl")
	baseTmpl := template.New("base")
	files, _ := filepath.Glob(pattern)

	if len(files) > 0 {
		template.Must(baseTmpl.ParseFiles(files...))
	}

	// Everything that doesn't start with `_` is assumed to be used for a view
	pattern = filepath.Join(dir, "*", "[^_]*.tmpl")
	files, _ = filepath.Glob(pattern)

	templates := make(map[string]*template.Template, len(files))

	for _, f := range files {
		name := strings.TrimPrefix(f, dir)
		name = strings.TrimSuffix(name, ".tmpl")

		tmpl := template.Must(baseTmpl.Clone())
		templates[name] = template.Must(tmpl.ParseFiles(f))
		fmt.Printf("registering template: %s\n", name)
	}

	return templates
}

const (
	authCookieName = "u"
)

func (s *Service) signCookie(value string) string {
	mac := hmac.New(sha256.New, s.cookieSecret)
	mac.Write([]byte(value))
	sum := mac.Sum(nil)
	return string(hex.EncodeToString(sum))
}

// returns (user_id, verified?)
func (s *Service) verifyCookie(c *http.Cookie) (int, bool) {
	// Check for expired cookies first
	if c.Expires.After(time.Now()) {
		return 0, false
	}

	// Next try to verify
	values := strings.SplitN(c.Value, ".", 2)
	if len(values) != 2 {
		return 0, false
	}

	given := []byte(values[1])
	expected := []byte(s.signCookie(values[0]))

	if hmac.Equal(expected, given) {
		uid, _ := strconv.Atoi(values[0])
		return uid, true
	}

	return 0, false
}

func (s *Service) setLoginUser(w http.ResponseWriter, u *UserModel) {
	signed := s.signCookie(strconv.Itoa(u.Id))
	value := fmt.Sprintf("%d.%s", u.Id, signed)

	http.SetCookie(w, &http.Cookie{
		Name:  authCookieName,
		Value: value,
	})
}

func (s *Service) loggedInUser(req *http.Request) *UserModel {
	if cookie, err := req.Cookie(authCookieName); err != nil {
		return nil
	} else if uid, ok := s.verifyCookie(cookie); !ok {
		return nil
	} else if user, err := s.db.UserById(uid); err != nil {
		fmt.Printf("no user with id: %d\n", uid)
		return nil
	} else {
		return user
	}
}

func (s *Service) renderHTML(w http.ResponseWriter, statusCode int, name string, data interface{}) {
	w.WriteHeader(statusCode)
	w.Header().Add("Content-Type", "text/html")
	tmpl, ok := s.templates[name]

	if !ok {
		panic(fmt.Errorf("tried to render unknown template: %s", name))
	} else if err := tmpl.Execute(w, data); err != nil {
		panic(fmt.Errorf("failed to render template: %+v", err))
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

		// Proceed with our modified request
		h(w, r2)
	})
}

func (s *Service) Serve() {
	mux := MiniMux{http.NewServeMux()}

	// TODO: hook up request logging

	mux.HandleScoped("/user", s.handleUserResource)
	mux.HandleScoped("/session/", s.handleSessionResource)
	mux.HandleScoped("/recipe/", s.handleRecipeResource)
	mux.HandleScoped("/tag/", s.handleTagResource)

	mux.HandleFunc("/recipe/edit", s.handleRecipeEdit)

	mux.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		if req.URL.Path != "/" {
			http.NotFound(w, req)
			return
		}

		s.handleListing(w, req)
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
		if err := s.db.NewUser(email, password); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}

		http.Redirect(w, req, "/", http.StatusSeeOther)

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

// POST /session/login
// GET  /session/logout
func (s *Service) handleSessionResource(w http.ResponseWriter, req *http.Request) {
	path := req.URL.Path

	switch {
	case req.Method == http.MethodPost && path == "login":
		email := req.FormValue("email")
		password := req.FormValue("password")

		user, err := s.db.UserByEmailAndPassword(email, password)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}

		s.setLoginUser(w, user)
		http.Redirect(w, req, "/", http.StatusSeeOther)

	case req.Method == http.MethodGet && path == "logout":
		http.SetCookie(w, &http.Cookie{
			Name:  authCookieName,
			Value: "",
		})

		http.Redirect(w, req, "/", http.StatusSeeOther)

	default:
		http.NotFound(w, req)
	}

}

// POST   /recipe
// GET    /recipe/:id
// PUT    /recipe/:id
// GET    /recipe/:id/edit
// DELETE /recipe/:id
func (s *Service) handleRecipeResource(w http.ResponseWriter, req *http.Request) {
	type updateRecipeBody struct{}

	path := req.URL.Path
	user := s.loggedInUser(req)

	// POST /recipe - create a new recipe
	if path == "" {
		if req.Method != http.MethodPost {
			http.NotFound(w, req)
			return
		} else if user == nil {
			sendStatus(w, http.StatusUnauthorized)
			return
		}

		r := RecipeModel{
			UserId: user.Id,
			Title:  req.FormValue("title"),
			Description: sql.NullString{
				String: req.FormValue("description"),
				Valid:  true,
			},
			Category: sql.NullString{
				String: req.FormValue("category"),
				Valid:  true,
			},
			Notes: sql.NullString{
				String: req.FormValue("notes"),
				Valid:  true,
			},
			Instructions: req.FormValue("instructions"),
			Ingredients:  req.FormValue("ingredients"),
		}

		if !r.Validate() {
			sendStatus(w, http.StatusBadRequest)
			return
		}

		fmt.Printf("Creating this recipe: %+v\n", r)

		id, err := s.db.NewRecipe(r)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
		}

		url := fmt.Sprintf("/recipe/%d", id)
		http.Redirect(w, req, url, http.StatusSeeOther)
		return
	}

	id, err := strconv.Atoi(path)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	recipe, err := s.db.RecipeById(id)
	if err != nil {
		http.NotFound(w, req)
		return
	}

	switch req.Method {
	case http.MethodGet:
		fmt.Printf("GET recipe %d: %+v\n", id, recipe)
		s.renderHTML(w, http.StatusOK, "recipe/show.html", recipe.View())

	case http.MethodPut:
		fmt.Printf("UPDATE recipe %d\n", id)
		s.renderHTML(w, http.StatusOK, "recipe/edit.html", recipe.View())

	case http.MethodDelete:
		fmt.Printf("DELETE recipe %d\n", id)

	default:
		http.NotFound(w, req)
	}
}

func (s *Service) handleRecipeEdit(w http.ResponseWriter, req *http.Request) {
	query := req.URL.Query()

	if ids, ok := query["id"]; !ok {
		http.Error(w, "missing id", http.StatusBadRequest)
	} else if id, err := strconv.Atoi(ids[0]); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
	} else if recipe, err := s.db.RecipeById(id); err != nil {
		http.NotFound(w, req)
	} else {
		s.renderHTML(w, http.StatusOK, "recipe/edit.html", recipe.View())
	}
}

// GET /tag
// GET /tag/:id
func (s *Service) handleTagResource(w http.ResponseWriter, req *http.Request) {
	fmt.Println("TODO: implement /tag")
	http.Error(w, "not implemented", 500)
}

// GET /
func (s *Service) handleListing(w http.ResponseWriter, req *http.Request) {
	fmt.Println("TODO: implement /")
	http.Error(w, "not implemented", 500)
}
