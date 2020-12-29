package main

import (
	"fmt"
	"net/http"
	"time"

	"github.com/gorilla/mux"
)

type Config struct {
	ListenAddr string
	StaticDir  string
	BuildDir   string
}

type Storage interface {
	JournalList() ([]JournalModel, error)
	JournalGet(int64) (*JournalModel, error)
	JournalCreate(*JournalModel) error
	JournalUpdate(*JournalModel) error
	JournalDelete(*JournalModel) error

	EntryList() ([]EntryModel, error)
	EntryGet(int64) (*EntryModel, error)
	EntryCreate(*EntryModel) error
	EntryUpdate(*EntryModel) error
	EntryDelete(*EntryModel) error

	MediaList() ([]MediaModel, error)
	MediaGet(int64) (*MediaModel, error)
	MediaCreate(*MediaModel) error
	MediaUpdate(*MediaModel) error
	MediaDelete(*MediaModel) error
}

type JournalModel struct {
	ID          int64
	Title       string
	Description string
	URL         string
	StartedAt   time.Time
	CompletedAt time.Time

	CreatedAt time.Time
	UpdatedAt time.Time
	DeletedAt time.Time
}

type EntryModel struct {
	ID           int64
	CollectionID int64
	URL          string
	Parts        []EntryPartWrapper

	CreatedAt time.Time
	UpdatedAt time.Time
	DeletedAt time.Time
}

type MediaModel struct {
	ID          int64
	EntryID     int64
	Name        string
	PublicURL   string
	Size        int64
	ContentType string
	IsCover     bool
	IsPublic    bool

	CreatedAt time.Time
	UpdatedAt time.Time
	DeletedAt time.Time
}

type EntryPartKind string

const (
	Markdown EntryPartKind = "markdown"
	Media                  = "media"
	Gallery                = "gallery"
)

type EntryPartWrapper struct {
	Kind EntryPartKind `json:"kind"`
	Body EntryPart     `json:"body"`
}

type EntryPart interface {
	Kind() EntryPartKind
	ToHTML() string
}

type MarkdownEntry struct {
	Text string `json:"text"`
}

type MediaEntry struct {
	MediaID      int64  `json:"media_id"`
	CaptionTitle string `json:"caption_title"`
	Caption      string `json:"caption"`
	FullWidth    bool   `json:"full_width"`
	FullHeight   bool   `json:"full_height"`
}

type GalleryEntry struct {
	Title   string       `json:"title"`
	Caption string       `json:"caption"`
	Media   []MediaEntry `json:"media"`
}

func main() {
	config := Config{
		ListenAddr: "127.0.0.1:8080",
		StaticDir:  "static/",
		BuildDir:   "/tmp/output",
	}

	router := NewRouter(config)
	http.Handle("/", router)

	fmt.Printf("Starting up! %+v\n", config)
	http.ListenAndServe(config.ListenAddr, nil)
}

func NewRouter(c Config) *mux.Router {
	router := mux.NewRouter()

	serveAPI(router, c)
	serveUI(router, c)

	return router
}

func serveAPI(router *mux.Router, c Config) {
	handler := APIHandler{c}
	sr := router.PathPrefix("/api/v1").Subrouter()

	// TODO: authentication middleware here

	sr.HandleFunc("/media", handler.createMedia).Methods(http.MethodPost)

	sr.HandleFunc("/journal", handler.listJournals).Methods(http.MethodGet)
	sr.HandleFunc("/journal", handler.createJournal).Methods(http.MethodPost)
	sr.HandleFunc("/journal/{ID}", handler.getJournal).Methods(http.MethodGet)
	sr.HandleFunc("/journal/{ID}", handler.editJournal).Methods(http.MethodPut)
	sr.HandleFunc("/journal/{ID}", handler.deleteJournal).Methods(http.MethodDelete)

	sr.HandleFunc("/entry", handler.listEntries).Methods(http.MethodGet)
	sr.HandleFunc("/entry", handler.createEntry).Methods(http.MethodPost)
	sr.HandleFunc("/entry/{entryID}", handler.getEntry).Methods(http.MethodGet)
	sr.HandleFunc("/entry/{entryID}", handler.editEntry).Methods(http.MethodPut)
	sr.HandleFunc("/entry/{entryID}", handler.deleteEntry).Methods(http.MethodDelete)
}

func serveUI(router *mux.Router, c Config) {
	sr := router.NewRoute().Subrouter()

	sr.PathPrefix("/static/").
		Handler(http.StripPrefix("/static/",
			http.FileServer(http.Dir(c.StaticDir))))

	sr.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		http.ServeFile(w, req, "index.html")
	})
}

type APIHandler struct {
	config Config
}

// Media

func (*APIHandler) createMedia(w http.ResponseWriter, req *http.Request) {}

// Journals

func (*APIHandler) listJournals(w http.ResponseWriter, req *http.Request)  {}
func (*APIHandler) getJournal(w http.ResponseWriter, req *http.Request)    {}
func (*APIHandler) createJournal(w http.ResponseWriter, req *http.Request) {}
func (*APIHandler) editJournal(w http.ResponseWriter, req *http.Request)   {}
func (*APIHandler) deleteJournal(w http.ResponseWriter, req *http.Request) {}

// Entries

func (*APIHandler) listEntries(w http.ResponseWriter, req *http.Request) {}
func (*APIHandler) getEntry(w http.ResponseWriter, req *http.Request)    {}
func (*APIHandler) createEntry(w http.ResponseWriter, req *http.Request) {}
func (*APIHandler) editEntry(w http.ResponseWriter, req *http.Request)   {}
func (*APIHandler) deleteEntry(w http.ResponseWriter, req *http.Request) {}
