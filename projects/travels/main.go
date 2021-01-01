package main

import (
	"crypto/rand"
	"encoding/base32"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/gorilla/mux"
)

func GenerateRandomString(length int) string {
	bytes := make([]byte, length)
	if _, err := rand.Read(bytes); err != nil {
		panic(err)
	}
	return base32.StdEncoding.EncodeToString(bytes)
}

type Config struct {
	ListenAddr string
	StaticDir  string
	BuildDir   string
}

type Ordering struct {
	Key       string
	Ascending bool
}

type Storage interface {
	JournalList(Ordering) ([]JournalModel, error)
	JournalGet(string) (*JournalModel, error)
	JournalUpsert(*JournalModel) error
	JournalDelete(*JournalModel) error

	EntryList(Ordering) ([]EntryModel, error)
	EntryGet(string) (*EntryModel, error)
	EntryUpsert(*EntryModel) error
	EntryDelete(*EntryModel) error

	MediaList(Ordering) ([]MediaModel, error)
	MediaGet(string) (*MediaModel, error)
	MediaUpsert(*MediaModel) error
	MediaDelete(*MediaModel) error
}

type FlatStorage struct {
	Dir      string
	Journals map[string]JournalModel
	Entries  map[string]EntryModel
	Media    map[string]MediaModel

	// TODO: metadata for the whole site?
	// Configuration map[string]string
}

func NewFlatStorage(dir string) (*FlatStorage, error) {
	fs := &FlatStorage{Dir: dir}

	jd := filepath.Join(dir, "journals")
	if err := os.MkdirAll(jd, os.ModeDir); err != nil {
		return nil, err
	}

	if err := fs.slurpJournals(jd); err != nil {
		return nil, err
	}

	return fs, nil
}

func (fs *FlatStorage) slurpJournals(journalsDir string) error {
	items, err := ioutil.ReadDir(journalsDir)
	if err != nil {
		return err
	}

	for _, item := range items {
		if !item.IsDir() {
			fmt.Printf("WARN, unknown file in journals directory: %+v\n", item)
			continue
		}

		jd := filepath.Join(journalsDir, item.Name())
		if err = fs.slurpJournalContent(jd); err != nil {
			return nil
		}
	}

	return nil
}

func slurpJSON(dir string, name string, receiver interface{}) error {
	path := filepath.Join(dir, name)
	text, err := ioutil.ReadFile(path)
	if err != nil {
		return err
	}

	if err = json.Unmarshal(text, receiver); err != nil {
		return err
	}

	return nil
}

func (fs *FlatStorage) slurpJournalContent(journalDir string) error {

	journal := JournalModel{}
	if err := slurpJSON(journalDir, "journal.json", &journal); err != nil {
		return err
	} else {
		fs.Journals[journal.ID] = journal
	}

	media := []MediaModel{}
	if err := slurpJSON(journalDir, "media.json", &media); err != nil {
		return err
	} else {
		for _, m := range media {
			fs.Media[m.ID] = m
		}
	}

	entriesPath := filepath.Join(journalDir, "entries")
	if err := fs.slurpJournalEntries(entriesPath); err != nil {
		return err
	}

	return nil
}

func (fs *FlatStorage) slurpJournalEntries(entriesPath string) error {
	items, err := ioutil.ReadDir(entriesPath)
	if err != nil {
		return err
	}

	for _, entFile := range items {
		if entFile.IsDir() || !strings.HasSuffix(entFile.Name(), ".json") {
			fmt.Println("Skipping unexpected item inside entries directory")
			continue
		}

		entry := EntryModel{}
		if err = slurpJSON(entriesPath, entFile.Name(), &entry); err != nil {
			return err
		}

		fs.Entries[entry.ID] = entry
	}

	return nil
}

// TODO -
// func (fs *FlatStorage) JournalList(ord Ordering) ([]JournalModel, error) {}

type JournalModel struct {
	ID          string
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
	ID           string
	CollectionID string
	URL          string
	Parts        []EntryPartWrapper

	CreatedAt time.Time
	UpdatedAt time.Time
	DeletedAt time.Time
}

type MediaModel struct {
	ID        string
	JournalID string
	EntryID   string

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
	Markdown    EntryPartKind = "markdown"
	InlineMedia               = "inline-media"
	// Gallery                = "gallery"
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

type InlineMediaEntry struct {
	MediaID      string `json:"media_id"`
	CaptionTitle string `json:"caption_title"`
	Caption      string `json:"caption"`
	FullWidth    bool   `json:"full_width"`
	FullHeight   bool   `json:"full_height"`
}

// TODO
// type GalleryEntry struct {
// 	Title   string             `json:"title"`
// 	Caption string             `json:"caption"`
// 	Media   []InlineMediaEntry `json:"media"`
// }

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

	sr.HandleFunc("/media", handler.listMedia).Methods(http.MethodGet)
	sr.HandleFunc("/media", handler.createMedia).Methods(http.MethodPost)
	sr.HandleFunc("/media/{ID}", handler.editMedia).Methods(http.MethodPut)

	sr.HandleFunc("/journals", handler.listJournals).Methods(http.MethodGet)
	sr.HandleFunc("/journals", handler.createJournal).Methods(http.MethodPost)
	sr.HandleFunc("/journals/{ID}", handler.getJournal).Methods(http.MethodGet)
	sr.HandleFunc("/journals/{ID}", handler.editJournal).Methods(http.MethodPut)
	sr.HandleFunc("/journals/{ID}", handler.deleteJournal).Methods(http.MethodDelete)

	sr.HandleFunc("/entries", handler.listEntries).Methods(http.MethodGet)
	sr.HandleFunc("/entries", handler.createEntry).Methods(http.MethodPost)
	sr.HandleFunc("/entries/{entryID}", handler.getEntry).Methods(http.MethodGet)
	sr.HandleFunc("/entries/{entryID}", handler.editEntry).Methods(http.MethodPut)
	sr.HandleFunc("/entries/{entryID}", handler.deleteEntry).Methods(http.MethodDelete)
}

func serveUI(router *mux.Router, c Config) {
	handler := UIHandler{c}
	sr := router.NewRoute().Subrouter()

	sr.PathPrefix("/static/").
		Handler(http.StripPrefix("/static/",
			http.FileServer(http.Dir(c.StaticDir))))

	sr.HandleFunc("/journals", handler.listJournals).Methods(http.MethodGet)
	sr.HandleFunc("/journals/{ID}", handler.showJournal).Methods(http.MethodGet)
	sr.HandleFunc("/journals/{ID}/{ID}", handler.showEntry).Methods(http.MethodGet)

	sr.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		http.ServeFile(w, req, "index.html")
	})
}

type UIHandler struct {
	config Config
}

func (*UIHandler) listJournals(w http.ResponseWriter, req *http.Request) {}
func (*UIHandler) showJournal(w http.ResponseWriter, req *http.Request)  {}
func (*UIHandler) showEntry(w http.ResponseWriter, req *http.Request)    {}

type APIHandler struct {
	config Config
}

// Media

func (*APIHandler) listMedia(w http.ResponseWriter, req *http.Request)   {}
func (*APIHandler) createMedia(w http.ResponseWriter, req *http.Request) {}
func (*APIHandler) editMedia(w http.ResponseWriter, req *http.Request)   {}

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
