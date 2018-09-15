// Various structs that are serialized into the database.
package model

import (
	"path/filepath"
	"regexp"
	"strings"

	"golang.org/x/oauth2"
)

type Post struct {
	Slug     string
	Title    string
	Subtitle string
	Author   string
	Date     string

	Content    string
	ImagePaths []string
}

var (
	NonAlnumChars     = regexp.MustCompile("[^a-z0-9]")
	NonAlnumPathChars = regexp.MustCompile("[^/a-z0-9]")
)

func (p Post) GenerateSlug(path string) string {
	path = strings.ToLower(path)
	title := strings.ToLower(p.Title)

	path = NonAlnumPathChars.ReplaceAllString(path, "-")
	title = NonAlnumChars.ReplaceAllString(title, "-")

	return filepath.Join(path, title)
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
	BasePath   string       `json:"base_path"`
	Host       string       `json:"host"`
	Pages      []PageConfig `json:"pages"`
	ContentDir string       `json:"content_dir"`
	Theme      string       `json:"theme"`

	Drive *SiteDriveConfig `json:"drive"`
}

func (s Site) HostPathPrefix() string { return s.Host + s.BasePath }
