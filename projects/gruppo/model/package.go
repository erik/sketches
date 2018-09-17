// Various structs that are serialized into the database.
package model

import (
	"crypto/sha256"
	"fmt"
	"path/filepath"
	"regexp"
	"strings"

	"golang.org/x/oauth2"
)

type Post struct {
	PostOverview

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

	slug := filepath.Join(path, title)

	return strings.Trim(slug, "-")
}

func (p Post) Overview() PostOverview {
	return p.PostOverview
}

type PostOverview struct {
	Slug     string
	Title    string
	Subtitle string
	Author   string
	Date     string

	Intro string
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
