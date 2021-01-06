package storage

import (
	"crypto/rand"
	"encoding/base32"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/erik/travels/model"
)

type journalWithEntries struct {
	Path string

	Journal model.Journal
	Entries map[string]model.Entry
	Media   map[string]model.Media
}

func (j journalWithEntries) SyncUpdates() error {
	fmt.Println("TODO - implement journalWithEntries.SyncUpdates()")

	return nil
}

type FlatStorage struct {
	Dir string

	// JournalID -> journal
	Journals map[string]journalWithEntries

	// TODO: metadata for the whole site?
	// Configuration map[string]string
}

// TODO: remove the panics? are these recoverable?
func NewFlatStorage(dir string) FlatStorage {
	fs := FlatStorage{
		Dir:      dir,
		Journals: map[string]journalWithEntries{},
	}

	jd := filepath.Join(dir, "journals")

	// TODO: What's the appropriate mode for this?
	if err := os.MkdirAll(jd, os.ModeDir|0700); err != nil {
		panic(err)
	}

	if err := fs.slurpJournals(jd); err != nil {
		panic(err)
	}

	return fs
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
			return err
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
	journal := model.Journal{}
	if err := slurpJSON(journalDir, "journal.json", &journal); err != nil {
		return err
	}

	media := map[string]model.Media{}
	if err := slurpJSON(journalDir, "media.json", &media); err != nil {
		return err
	}

	entriesPath := filepath.Join(journalDir, "entries")
	entries, err := fs.slurpJournalEntries(entriesPath)
	if err != nil {
		return err
	}

	fs.Journals[journal.ID] = journalWithEntries{
		Path:    journalDir,
		Journal: journal,
		Media:   media,
		Entries: *entries,
	}

	return nil
}

func (fs *FlatStorage) slurpJournalEntries(entriesPath string) (*map[string]model.Entry, error) {
	items, err := ioutil.ReadDir(entriesPath)
	if err != nil {
		return nil, err
	}

	entries := map[string]model.Entry{}

	for _, entFile := range items {
		if entFile.IsDir() || !strings.HasSuffix(entFile.Name(), ".json") {
			fmt.Println("Skipping unexpected item inside entries directory")
			continue
		}

		entry := model.Entry{}
		if err = slurpJSON(entriesPath, entFile.Name(), &entry); err != nil {
			return nil, err
		}
		entries[entry.ID] = entry
	}

	return &entries, nil
}

func (fs *FlatStorage) journalPath(journal model.Journal) string {
	// TODO: For now, keep it simple, but eventually it would be
	// nice to have these more appropriately ordered.
	//
	// e.g. journals/2020_12_31_[id hash]_this_is_my_post_title_slug
	return filepath.Join(fs.Dir, "journals", journal.ID)
}

func (fs *FlatStorage) JournalList() ([]model.Journal, error) {
	journals := make([]model.Journal, len(fs.Journals))

	for _, j := range fs.Journals {
		journals = append(journals, j.Journal)
	}

	return journals, nil
}

func (fs *FlatStorage) JournalGetByID(ID string) (*model.Journal, error) {
	if j, ok := fs.Journals[ID]; ok {
		copy := j.Journal
		return &copy, nil
	}

	return nil, errors.New("unknown journal ID")
}

func (fs *FlatStorage) JournalUpsert(update model.Journal) error {
	// If we don't have an ID yet, this is an insert
	if update.ID == "" {
		update.ID = genRandomID(32)

		fs.Journals[update.ID] = journalWithEntries{
			Path: fs.journalPath(update),

			Journal: update,
			Entries: map[string]model.Entry{},
			Media:   map[string]model.Media{},
		}
	} else if existing, ok := fs.Journals[update.ID]; ok {
		// updating existing record
		existing.Journal = update
		fs.Journals[update.ID] = existing
	} else {
		return errors.New("trying to update a non-existent journal")
	}

	return nil
}

func (fs *FlatStorage) JournalDelete(model.Journal) error { panic("not implemented") }

func (fs *FlatStorage) EntryListByJournalID(journalID string) ([]model.Entry, error) {
	journal, found := fs.Journals[journalID]
	if !found {
		return []model.Entry{}, errors.New("unknown journal ID")
	}

	// TODO: can we copy without a loop?
	entries := make([]model.Entry, len(journal.Entries))
	for _, ent := range journal.Entries {
		entries = append(entries, ent)
	}

	return entries, nil
}

func (fs *FlatStorage) EntryGetByID(journalID, entryID string) (*model.Entry, error) {
	journal, found := fs.Journals[journalID]
	if !found {
		return nil, errors.New("unknown journal ID")
	}

	for _, ent := range journal.Entries {
		if ent.ID == entryID {
			copy := ent
			return &copy, nil
		}
	}

	return nil, errors.New("unknown entry ID")
}

func (fs *FlatStorage) EntryUpsert(update model.Entry) error {
	journalWithEntries, found := fs.Journals[update.JournalID]
	if !found {
		return errors.New("unknown journal ID")
	}

	// If we don't have an ID yet, this is an insert
	if update.ID == "" {
		update.ID = genRandomID(32)
	} else if _, found := journalWithEntries.Entries[update.ID]; !found {
		return errors.New("trying to update a non-existent entry")
	}

	journalWithEntries.Entries[update.ID] = update
	fs.Journals[update.JournalID] = journalWithEntries

	return nil
}

func (fs *FlatStorage) EntryDelete(model.Entry) error { panic("not implemented") }

func (fs *FlatStorage) MediaListByJournalID(journalID string) ([]model.Media, error) {
	journal, found := fs.Journals[journalID]
	if !found {
		return []model.Media{}, errors.New("unknown journal ID")
	}

	// TODO: can we copy without a loop?
	media := make([]model.Media, len(journal.Media))
	for _, m := range journal.Media {
		media = append(media, m)
	}

	return media, nil
}
func (fs *FlatStorage) MediaGetByID(journalID, mediaID string) (*model.Media, error) {
	journal, found := fs.Journals[journalID]
	if !found {
		return nil, errors.New("unknown journal ID")
	}

	for _, m := range journal.Media {
		if m.ID == mediaID {
			copy := m
			return &copy, nil
		}
	}

	return nil, errors.New("unknown entry ID")
}

func (fs *FlatStorage) MediaUpsert(update model.Media) error {
	journalWithEntries, found := fs.Journals[update.JournalID]
	if !found {
		return errors.New("unknown journal ID")
	}

	// If we don't have an ID yet, this is an insert
	if update.ID == "" {
		update.ID = genRandomID(32)
	} else if _, found := journalWithEntries.Media[update.ID]; !found {
		return errors.New("trying to update a non-existent media")
	}

	journalWithEntries.Media[update.ID] = update
	fs.Journals[update.JournalID] = journalWithEntries

	return nil
}

func (fs *FlatStorage) MediaDelete(model.Media) error { panic("not implemented") }

func genRandomID(length int) string {
	bytes := make([]byte, length)
	if _, err := rand.Read(bytes); err != nil {
		panic(err)
	}
	return base32.StdEncoding.EncodeToString(bytes)
}
