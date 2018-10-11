package drive

import (
	"golang.org/x/oauth2"
	"google.golang.org/api/drive/v3"

	"github.com/erik/gruppo/model"
	"github.com/erik/gruppo/store"
	"github.com/erik/gruppo/util"
)

type Configuration struct {
	ClientId             string
	ClientSecret         string
	RedirectURI          string
	WatchNotificationURI string
}

type GoogleDriveProvider struct {
	oauth  oauth2.Config
	config Configuration
	db     *store.RedisStore
}

type Client struct {
	service *drive.Service
	config  Configuration
	db      *store.RedisStore
	site    model.Site
}

type File struct {
	Id          string
	Name        string        `json:",omitempty"`
	Path        string        `json:",omitempty"`
	Author      string        `json:",omitempty"`
	CreatedTime util.JSONTime `json:",omitempty"` // RFC 3339
}

type DriveChange struct {
	FileId string
	Path   string
}

// Either `File` or an error. It's like a union type, but fucking stupid.
type FileResult interface{}

const (
	mimeTypeDocx        = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
	mimeTypeDriveFolder = "application/vnd.google-apps.folder"
	mimeTypeDriveDoc    = "application/vnd.google-apps.document"
)
