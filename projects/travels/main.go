package main

import (
	"fmt"
	"net/http"

	"github.com/gorilla/mux"

	_ "github.com/erik/travels/model"
	_ "github.com/erik/travels/storage"
)

type Config struct {
	ListenAddr string
	StaticDir  string
	BuildDir   string
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
