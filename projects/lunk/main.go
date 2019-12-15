package main

import (
	"crypto/hmac"
	"crypto/sha256"
	"database/sql"
	"encoding/hex"
	"flag"
	"fmt"
	"log"
	"net/http"
	"strconv"
	"strings"
	"time"

	"github.com/gorilla/mux"
	"github.com/jmoiron/sqlx"
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
	// TODO: Where should this come from?
	cookieSecret := "asdf"
	db, err := NewDB(dbPath)
	if err != nil {
		log.Fatal("unable to connect to db", err)
	}

	db.addUser("blah", "blah")

	web := NewWeb(db, cookieSecret)
	web.Serve(bindHost)
}

type Web struct {
	db           *DB
	cookieSecret string
}

func NewWeb(db *DB, cookieSecret string) Web {
	return Web{db, cookieSecret}
}

func (w *Web) signCookie(value string) string {
	mac := hmac.New(sha256.New, []byte(w.cookieSecret))
	mac.Write([]byte(value))
	sum := mac.Sum(nil)
	return string(hex.EncodeToString(sum))
}

// TODO: Should just use JWTs or a regular session cookie backed by
// memory the DB instead of this handrolled stuff.
func (w *Web) verifyCookie(c *http.Cookie) (int, bool) {
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
	expected := []byte(w.signCookie(values[0]))

	if !hmac.Equal(expected, given) {
		return 0, false
	}

	values = strings.SplitN(values[0], ",", 2)
	uid, _ := strconv.Atoi(values[0])

	// Check for expiration
	now := time.Now().Unix()
	if expiration, err := strconv.ParseInt(values[1], 10, 64); err != nil || expiration < now {
		return 0, false
	}

	return uid, true
}

func (wb *Web) setLoginUser(w http.ResponseWriter, uid int) {
	expiration := time.Now().Add(365 * 24 * time.Hour).Unix()
	val := fmt.Sprintf("%d,%d", uid, expiration)
	signed := wb.signCookie(val)
	value := fmt.Sprintf("%s.%s", signed, val)

	http.SetCookie(w, &http.Cookie{Name: "u", Value: value})
}

func (w *Web) authMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// cookie := r.Cookie("u")
		// TODO: Verify auth here
		next.ServeHTTP(w, r)
	})
}

func (wb *Web) indexHandler(w http.ResponseWriter, r *http.Request) {
	pg, err := strconv.Atoi(r.URL.Query().Get("page"))
	if err != nil {
		pg = 0
	}

	lunks, err := wb.db.getLunks(pg)
	if err != nil {
		log.Printf("[error] failed to get links: %+v\n", err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	fmt.Printf("lunks => %+v\n", lunks)

	w.WriteHeader(200)
	w.Header().Add("Content-Type", "text/html")
}

func (wb *Web) loginHandler(w http.ResponseWriter, r *http.Request) {
	user := r.FormValue("user")
	password := r.FormValue("password")

	uid, err := wb.db.validateLogin(user, password)
	if err != nil {
		// TODO: Correct error response
		log.Printf("login failed: %+v\n", err)
		http.NotFound(w, r)
	}

	wb.setLoginUser(w, uid)
	http.Redirect(w, r, "/", http.StatusSeeOther)
}

func (wb *Web) logoutHandler(w http.ResponseWriter, r *http.Request) {
	http.SetCookie(w, &http.Cookie{Name: "u", Value: ""})
	http.Redirect(w, r, "/", http.StatusSeeOther)
}

func (w *Web) Serve(bindHost string) {
	mux := mux.NewRouter()
	mux.HandleFunc("/", w.indexHandler)

	mux.HandleFunc("/tags", w.indexHandler)
	mux.HandleFunc("/links/{linkId}", w.indexHandler)
	mux.HandleFunc("/search", w.indexHandler)

	mux.HandleFunc("/login", w.loginHandler).
		Methods("POST")

	mux.HandleFunc("/logout", w.logoutHandler).
		Methods("POST")

	// TODO: this
	// mux.PathPrefix("/static/").
	// 	Handler(http.StripPrefix("/static/", http.FileServer(http.Dir(dir))))

	log.Printf("[web] starting up on %s", bindHost)

	srv := &http.Server{
		Handler:      mux,
		Addr:         bindHost,
		WriteTimeout: 15 * time.Second,
		ReadTimeout:  15 * time.Second,
	}
	log.Fatal(srv.ListenAndServe())
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
  lunk_id INTEGER NOT NULL,
  tag     TEXT NOT NULL,

  FOREIGN KEY (lunk_id) REFERENCES lunks(id)
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_uniq_tags ON tags(lunk_id, tag);
-- Does sqlite use the above index for this already?
CREATE        INDEX IF NOT EXISTS idx_tags_by_lunk_id ON tags(lunk_id);
`,
}

type User struct {
	Id       int
	Username string
	PwHash   string
}

type Lunk struct {
	Id          int
	UserId      int `db:"user_id"`
	Url         string
	Description string
	via         sql.NullString
	CreatedAt   string `db:"created_at"`
	DeletedAt   string `db:"deleted_at"`
}

type Tag struct {
	Id     int
	LunkId int `db:"lunk_id"`
	Tag    string
}

type DB struct {
	conn *sqlx.DB
}

func NewDB(uri string) (*DB, error) {
	conn, err := sqlx.Connect("sqlite3", uri)
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

func (db DB) getLunks(page int) ([]Lunk, error) {
	lunks := []Lunk{}
	sql := `
SELECT *
FROM lunks
ORDER BY created_at DESC
OFFSET ?
LIMIT 100`

	err := db.conn.Select(&lunks, sql, 100*page)
	return lunks, err
}

func (db DB) validateLogin(user, pw string) (int, error) {
	if pwHash, err := hashPassword(pw); err != nil {
		return -1, err
	} else {
		var uid int
		sql := `SELECT id FROM users WHERE username = ? AND pw_hash = ?`
		err = db.conn.QueryRow(sql, user, pwHash).Scan(&uid)
		return uid, err
	}
}

func hashPassword(pw string) (string, error) {
	bytes, err := bcrypt.GenerateFromPassword([]byte(pw), 14)
	return string(bytes), err
}
