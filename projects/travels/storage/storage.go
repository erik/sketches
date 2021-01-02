package storage

import (
	"github.com/erik/travels/model"
)

type Storage interface {
	JournalList() ([]model.Journal, error)
	JournalGetByID(string) (*model.Journal, error)
	JournalUpsert(model.Journal) error
	JournalDelete(model.Journal) error

	EntryListByJournalID(string) ([]model.Entry, error)
	EntryGetByID(string, string) (*model.Entry, error)
	EntryUpsert(model.Entry) error
	EntryDelete(model.Entry) error

	MediaListByJournalID() ([]model.Media, error)
	MediaGetByID(string, string) (*model.Media, error)
	MediaUpsert(model.Media) error
	MediaDelete(model.Media) error
}
