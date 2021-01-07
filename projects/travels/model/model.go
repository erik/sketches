package model

import (
	"regexp"
	"time"
)

type Journal struct {
	ID          string     `json:"id"`
	Title       string     `json:"title"`
	Description string     `json:"description"`
	Slug        string     `json:"slug"`
	StartedAt   time.Time  `json:"started_at"`
	CompletedAt *time.Time `json:"completed_at"`

	CreatedAt time.Time  `json:"created_at"`
	UpdatedAt *time.Time `json:"updated_at"`
	DeletedAt *time.Time `json:"deleted_at"`
}

func NewJournal(title string, description string) Journal {
	now := time.Now()

	return Journal{
		ID:          "",
		Title:       title,
		Description: description,
		Slug:        SlugifyTitle(title),

		StartedAt:   now,
		CompletedAt: nil,

		CreatedAt: now,
		UpdatedAt: nil,
		DeletedAt: nil,
	}
}

type Entry struct {
	ID        string             `json:"id"`
	JournalID string             `json:"journal_id"`
	Title     string             `json:"title"`
	Parts     []EntryPartWrapper `json:"parts"`
	Slug      string             `json:"slug"`

	CreatedAt time.Time
	UpdatedAt *time.Time
	DeletedAt *time.Time
}

func NewEntry(journalID string, title string, parts []EntryPartWrapper) Entry {
	return Entry{
		ID:        "",
		JournalID: journalID,
		Title:     title,
		Parts:     parts,
		Slug:      SlugifyTitle(title),

		CreatedAt: time.Now(),
		UpdatedAt: nil,
		DeletedAt: nil,
	}
}

type Media struct {
	ID        string `json:"id"`
	JournalID string `json:"journal_id"`
	EntryID   string `json:"entry_id"`

	Name        string `json:"name"`
	Size        int64  `json:"size"`
	Slug        string `json:"slug"`
	ContentType string `json:"content_type"`
	IsCover     bool   `json:"is_cover"`
	IsPublic    bool   `json:"is_public"`

	CreatedAt time.Time
	UpdatedAt time.Time
	DeletedAt time.Time
}

type EntryPartKind string

const (
	Markdown    EntryPartKind = "markdown"
	InlineMedia               = "inline-media"
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

var (
	nonAlnum     = regexp.MustCompile("[[:^alnum:]]")
	repeatedDash = regexp.MustCompile("-+")
)

func SlugifyTitle(t string) string {
	t = nonAlnum.ReplaceAllString(t, "-")
	return repeatedDash.ReplaceAllString(t, "-")
}
