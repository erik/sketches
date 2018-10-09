// Various structs that are serialized into the database.
package model

import (
	"crypto/sha256"
	"fmt"
	"path/filepath"
	"regexp"
	"strings"

	"golang.org/x/oauth2"

	"github.com/erik/gruppo/util"
)

type Post struct {
	PostOverview

	Content    string   `json:",omitempty"`
	ImagePaths []string `json:"-"`
}

var (
	nonAlnumChars     = regexp.MustCompile("[^a-z0-9]")
	nonAlnumPathChars = regexp.MustCompile("[^/a-z0-9]")
	repeatedDashes    = regexp.MustCompile("-+")
)

func (p Post) GenerateSlug(path, name string) string {
	path = strings.ToLower(path)
	title := strings.ToLower(name)

	path = nonAlnumPathChars.ReplaceAllString(path, "-")
	title = nonAlnumChars.ReplaceAllString(title, "-")

	slug := filepath.Join(path, title)
	slug = repeatedDashes.ReplaceAllString(slug, "-")
	slug = strings.Trim(slug, "-")

	if !strings.HasPrefix(slug, "/") {
		return "/" + slug
	}

	return slug
}

func (p Post) Overview() PostOverview {
	return p.PostOverview
}

type PostOverview struct {
	Slug        string
	Title       string        `json:",omitempty"`
	Subtitle    string        `json:",omitempty"`
	Author      string        `json:",omitempty"`
	PublishDate util.JSONTime `json:",omitempty"`

	Intro string `json:",omitempty"`
}

type SiteDriveConfig struct {
	FolderId string        `json:"folder_id"`
	Token    *oauth2.Token `json:"token"`
}

type PageConfig struct {
	URL      string `json:"url"`
	Template string `json:"template"`
	Title    string `json:"title"`
	Post     string `json:"post_slug"`
}

type Site struct {
	BasePath  string       `json:"base_path"`
	AssetPath string       `json:"asset_path"`
	Host      string       `json:"host"`
	IndexPage *PageConfig  `json:"index_page"`
	Pages     []PageConfig `json:"pages"`
	SiteDir   string       `json:"site_dir"`
	Theme     string       `json:"theme"`

	Drive *SiteDriveConfig `json:"drive"`
}

func (s Site) HostPathPrefix() string { return s.Host + s.BasePath }

func (s Site) WebhookKey() string {
	sum := sha256.Sum256([]byte(s.HostPathPrefix()))
	return fmt.Sprintf("%x", sum)
}
