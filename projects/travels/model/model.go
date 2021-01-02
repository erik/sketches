package model

import (
	"time"
)

type Journal struct {
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

type Entry struct {
	ID        string
	JournalID string
	URL       string
	Parts     []EntryPartWrapper

	CreatedAt time.Time
	UpdatedAt time.Time
	DeletedAt time.Time
}

type Media struct {
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
