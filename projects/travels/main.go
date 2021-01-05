package main

import (
	"bytes"
	"fmt"
	"html/template"
	"net/http"
	"path/filepath"

	"github.com/gorilla/mux"

	"github.com/erik/travels/storage"
)

type Config struct {
	ListenAddr     string
	StaticDir      string
	BuildDir       string
	FlatStorageDir string
}

type AppContext struct {
	Config Config
	Store  storage.Storage // TODO: rename this to storage.Store
}

func BuildAppContext(conf Config) AppContext {
	// TODO: this needs more configurability
	store := storage.NewFlatStorage(
		conf.FlatStorageDir,
	)

	return AppContext{
		Config: conf,
		Store:  &store,
	}
}

func main() {
	// TODO: Read this from environment
	config := Config{
		ListenAddr:     "127.0.0.1:8080",
		StaticDir:      "static/",
		BuildDir:       "/tmp/journals/output",
		FlatStorageDir: "/tmp/journals/input",
	}

	appCtx := BuildAppContext(config)

	router := NewRouter(&appCtx)
	http.Handle("/", router)

	fmt.Printf("Starting up! %+v\n", config)
	http.ListenAndServe(config.ListenAddr, nil)
}

func NewRouter(appCtx *AppContext) *mux.Router {
	router := mux.NewRouter()

	serveAPI(router, appCtx)
	serveUI(router, appCtx)

	return router
}

func serveAPI(router *mux.Router, appCtx *AppContext) {
	handler := APIHandler{appCtx}
	sr := router.PathPrefix("/api/v1").Subrouter()

	// TODO: authentication middleware here
	// TODO: likely most of these routes will not be required

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

func serveUI(router *mux.Router, appCtx *AppContext) {
	handler := UIHandler{appCtx}
	sr := router.NewRoute().Subrouter()

	sr.PathPrefix("/static/").
		Handler(http.StripPrefix("/static/",
			http.FileServer(http.Dir(appCtx.Config.StaticDir))))

	sr.HandleFunc("/journals", handler.showJournalList).Methods(http.MethodGet)
	sr.HandleFunc("/journals/{ID}", handler.showJournalEntries).Methods(http.MethodGet)
	sr.HandleFunc("/journals/{ID}/{ID}", handler.showEntry).Methods(http.MethodGet)

	sr.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		http.ServeFile(w, req, "index.html")
	})
}

func SimpleError(w http.ResponseWriter, req *http.Request, err error) {
	// TODO: implement this
	panic(err)
}

type UIHandler struct{ *AppContext }

// TODO: Need a more serious wrapper around this eventually
var templates = make(map[string]*template.Template)

func init() {
	names := []string{
		"journal_list",
	}

	commonFiles, err := filepath.Glob("template/common/*")
	if err != nil {
		panic(err)
	}

	for _, name := range names {
		fileName := fmt.Sprintf("%s.html", name)
		path := filepath.Join("template/", fileName)
		templates[name] = template.Must(
			template.New(fileName).ParseFiles(append(commonFiles, path)...),
		)
	}
}

func renderHTML(tplName string, tplData map[string]interface{}, w http.ResponseWriter, req *http.Request) {
	tpl, ok := templates[tplName]
	if !ok {
		panic("unknown template")
	}

	var output bytes.Buffer
	if err := tpl.ExecuteTemplate(&output, "base", tplData); err != nil {
		SimpleError(w, req, err)
		return
	}

	w.Header().Set("Content-Type", "text/html")
	w.Write(output.Bytes())
}

func (h *UIHandler) showJournalList(w http.ResponseWriter, req *http.Request) {
	journals, err := h.Store.JournalList()
	if err != nil {
		SimpleError(w, req, err)
		return
	}

	vars := make(map[string]interface{})
	vars["journals"] = journals

	renderHTML("journal_list", vars, w, req)
}

func (*UIHandler) showJournalEntries(w http.ResponseWriter, req *http.Request) {}
func (*UIHandler) showEntry(w http.ResponseWriter, req *http.Request)          {}

type APIHandler struct{ *AppContext }

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
