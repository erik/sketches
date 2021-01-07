package main

import (
	"bytes"
	"fmt"
	"html/template"
	"net/http"
	"path/filepath"

	"github.com/gorilla/mux"

	"github.com/erik/travels/model"
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

func BuildAppContext(c Config) AppContext {
	// TODO: this needs more configurability
	s := storage.NewFlatStorage(
		c.FlatStorageDir,
	)

	return AppContext{
		Config: c,
		Store:  &s,
	}
}

// TODO: remove this - for testing
func seedStorage(s storage.Storage) {
	for i := 0; i < 5; i++ {
		jrnl := model.NewJournal(
			fmt.Sprintf("Journal #%d", i),
			fmt.Sprintf("A description #%s", i),
		)
		if err := s.JournalUpsert(&jrnl); err != nil {
			panic(err)
		}
		fmt.Printf("- %+v\n", jrnl)

		for j := 0; j < 5; j++ {
			e := model.NewEntry(
				jrnl.ID,
				fmt.Sprintf("Entry %d", j),
				[]model.EntryPartWrapper{},
			)
			if err := s.EntryUpsert(&e); err != nil {
				panic(err)
			}

			fmt.Printf("  - %+v\n", e)
		}
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

	seedStorage(appCtx.Store)

	router := NewRouter(&appCtx)
	http.Handle("/", router)

	fmt.Printf("Starting up! %+v\n", config)
	http.ListenAndServe(config.ListenAddr, nil)
}

func NewRouter(appCtx *AppContext) *mux.Router {
	r := mux.NewRouter()

	serveAPI(r, appCtx)
	serveUI(r, appCtx)

	return r
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

	sr.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		http.Redirect(w, r, "/journals", http.StatusSeeOther)
	})
}

func SimpleError(w http.ResponseWriter, req *http.Request, err error) {
	// TODO: implement this
	panic(err)
}

type TemplateContext struct {
	tpl  *template.Template
	data map[string]interface{}
}

func NewTemplateContext(tplName string) TemplateContext {
	tpl, ok := templates[tplName]
	if !ok {
		panic("unknown template")
	}

	return TemplateContext{
		tpl:  tpl,
		data: map[string]interface{}{},
	}
}

func (c *TemplateContext) Set(k string, v interface{}) {
	c.data[k] = v
}

func (c TemplateContext) RenderHTML(w http.ResponseWriter, r *http.Request) error {
	var output bytes.Buffer
	if err := c.tpl.ExecuteTemplate(&output, "base", c.data); err != nil {
		return err
	}

	w.Header().Set("Content-Type", "text/html")
	w.Write(output.Bytes())
	return nil
}

type UIHandler struct{ *AppContext }

// TODO: Need a more serious wrapper around this eventually
var templates = map[string]*template.Template{}

func init() {
	names := []string{
		"journal_list",
		"entry_list",
	}

	commonFiles, err := filepath.Glob("template/common/*")
	if err != nil {
		panic(err)
	}

	for _, n := range names {
		p := filepath.Join("template/", fmt.Sprintf("%s.html", n))
		templates[n] = template.Must(
			template.New("").ParseFiles(append(commonFiles, p)...),
		)
	}
}

func (h *UIHandler) showJournalList(w http.ResponseWriter, req *http.Request) {
	journals, err := h.Store.JournalList()
	if err != nil {
		SimpleError(w, req, err)
		return
	}

	ctx := NewTemplateContext("journal_list")
	ctx.Set("journals", journals)
	ctx.RenderHTML(w, req)
}

func (h *UIHandler) showJournalEntries(w http.ResponseWriter, req *http.Request) {
	vars := mux.Vars(req)
	journalID, ok := vars["ID"]
	if !ok {
		err := fmt.Errorf("missing journalID")
		SimpleError(w, req, err)
		return
	}

	journal, err := h.Store.JournalGetByID(journalID)
	if err != nil {
		SimpleError(w, req, err)
		return
	}

	entries, err := h.Store.EntryListByJournalID(journal.ID)
	if err != nil {
		SimpleError(w, req, err)
		return
	}

	ctx := NewTemplateContext("entry_list")
	ctx.Set("journal", journal)
	ctx.Set("entries", entries)
	ctx.RenderHTML(w, req)
}

func (*UIHandler) showEntry(w http.ResponseWriter, req *http.Request) {

}

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
