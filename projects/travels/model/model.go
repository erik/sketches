package model

import (
	"time"
)

type Journal struct {
	ID          string    `json:"id"`
	Title       string    `json:"title"`
	Description string    `json:"description"`
	URL         string    `json:"url"`
	StartedAt   time.Time `json:"started_at"`
	CompletedAt time.Time `json:"completed_at"`

	CreatedAt time.Time `json:"created_at"`
	UpdatedAt time.Time `json:"updated_at"`
	DeletedAt time.Time `json:"deleted_at"`
}

type Entry struct {
	ID        string             `json:"id"`
	JournalID string             `json:"journal_id"`
	URL       string             `json:"url"`
	Parts     []EntryPartWrapper `json:"parts"`

	CreatedAt time.Time
	UpdatedAt time.Time
	DeletedAt time.Time
}

type Media struct {
	ID        string `json:"id"`
	JournalID string `json:"journal_id"`
	EntryID   string `json:"entry_id"`

	Name        string `json:"name"`
	PublicURL   string `json:"public_url"`
	Size        int64  `json:"size"`
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
