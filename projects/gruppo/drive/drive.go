package drive

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"

	log "github.com/sirupsen/logrus"
	"golang.org/x/net/context"
	"golang.org/x/oauth2"
	"golang.org/x/oauth2/google"
	"google.golang.org/api/drive/v3"

	"github.com/erik/gruppo/converters"
	"github.com/erik/gruppo/model"
	"github.com/erik/gruppo/store"
)

const (
	MimeTypeDocx        = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
	MimeTypeDriveFolder = "application/vnd.google-apps.folder"
	MimeTypeDriveDoc    = "application/vnd.google-apps.document"
)

type Configuration struct {
	ClientId             string
	ClientSecret         string
	RedirectURI          string
	WatchNotificationURI string
}

type GoogleDriveProvider struct {
	oauth  *oauth2.Config
	config *Configuration
	db     *store.RedisStore
}

type Client struct {
	service *drive.Service
	config  *Configuration
	db      *store.RedisStore
	site    model.Site
}

type folder struct {
	path string
	id   string
}

type File struct {
	Id     string
	Name   string
	Path   string
	Author string
}

const (
	fileChangeCreated = "created"
	fileChangeUpdated = "updated"
	fileChangeDeleted = "deleted"
)

type FileChange struct {
	File File
	Kind string // one of "created", "deleted", "updated"
}

// Either `File` or an error. It's like a union type, but fucking stupid.
type FileResult interface{}

func NewGoogleDriveProvider(conf Configuration, db *store.RedisStore) GoogleDriveProvider {
	oauth := &oauth2.Config{
		ClientID:     conf.ClientId,
		ClientSecret: conf.ClientSecret,
		RedirectURL:  conf.RedirectURI,

		Scopes:   []string{drive.DriveReadonlyScope},
		Endpoint: google.Endpoint,
	}

	return GoogleDriveProvider{oauth, &conf, db}
}

func (p GoogleDriveProvider) ClientForSite(site model.Site) (*Client, error) {
	tok := site.Drive.Token

	client := p.oauth.Client(context.TODO(), tok)

	svc, err := drive.New(client)
	if err != nil {
		return nil, err
	}

	return &Client{
		service: svc,
		config:  p.config,
		db:      p.db,
		site:    site,
	}, nil
}

func (c Client) Start(forceSync bool, conf Configuration) {
	go c.changeWatcherRefresher(c.site.Drive.FolderId)
	go c.changeHandler()

	if forceSync {
		log.WithField("site", c.site.HostPathPrefix()).
			Info("starting force sync for site")

		if err := c.ForceSync(); err != nil {
			log.Fatal(err)
		}
	}
}

func (c Client) ExportFile(file File, dir string) (*model.Post, error) {
	docx, err := c.ExportAsDocx(file)
	if err != nil {
		return nil, err
	}

	path := filepath.Join(dir, file.Id)
	if err := os.Mkdir(path, os.ModePerm); err != nil {
		return nil, err
	}

	inputFileName := filepath.Join(path, "input.docx")
	inputFile, err := os.Create(inputFileName)
	if err != nil {
		return nil, err
	}

	w := bufio.NewWriter(inputFile)
	if _, err := io.Copy(w, docx); err != nil {
		return nil, err
	}
	if err := w.Flush(); err != nil {
		return nil, err
	}

	md, err := converters.ConvertDocx(inputFileName, path)
	if err != nil {
		return nil, err
	}

	post := converters.ExtractPost(md)
	post.Author = file.Author
	post.Slug = post.GenerateSlug(file.Path)

	if err := converters.HandlePostMedia(&c.site, &post); err != nil {
		return nil, err
	}

	return &post, nil
}

func (c Client) ProcessFile(file File, tmpDir string) (*model.Post, error) {
	post, err := c.ExportFile(file, tmpDir)
	if err != nil {
		log.WithError(err).
			WithField("site", c.site.HostPathPrefix()).
			Error("failed to export file from drive")

		return nil, err
	}

	go c.changeWatcherRefresher(file.Id)

	// If we're refreshing an existing post, don't change the slug
	slug, err := c.getOrSetSlugForFileId(file.Id, post.Slug)
	if err != nil {
		return nil, err
	}

	post.Slug = slug

	if err := c.db.AddPost(c.site, *post); err != nil {
		return nil, err
	}

	log.WithFields(log.Fields{
		"site": c.site.HostPathPrefix(),
		"slug": post.Slug,
	}).Debug("extracted post")

	return post, nil
}

func (c Client) ForceSync() error {
	dir, err := ioutil.TempDir("/tmp", "exported-media-")
	if err != nil {
		log.Fatal(err)
	}

	defer os.RemoveAll(dir)

	// Start with a clean slate
	if err := c.db.ClearSiteData(c.site); err != nil {
		return err
	}

	// Generated List of posts
	overview := []model.PostOverview{}

	for res := range c.List(c.site.Drive.FolderId) {
		file, ok := res.(File)
		if !ok {
			return res.(error)
		}

		post, err := c.ProcessFile(file, dir)
		if err != nil {
			return err
		}

		overview = append(overview, post.Overview())
	}

	return c.db.SetPostOverviews(c.site, overview)
}

func (c Client) ExportAsDocx(file File) (io.Reader, error) {
	log.WithFields(log.Fields{
		"site": c.site.HostPathPrefix(),
		"file": file.Name,
	}).Debug("exporting from drive as a .docx")

	// TODO: Support large file download via chunked transfer.
	res, err := c.service.Files.
		Export(file.Id, MimeTypeDocx).
		Download()

	if err != nil {
		return nil, err
	}

	return res.Body, nil
}

func (c Client) listFolder(rootId string, files chan FileResult) {
	defer close(files)

	var current folder

	// Ids of folders that we haven't explored yet
	folders := []folder{
		folder{id: rootId},
	}

	for len(folders) > 0 {
		current, folders = folders[0], folders[1:]

		log.WithFields(log.Fields{
			"site":   c.site.HostPathPrefix(),
			"folder": current,
		}).Debug("exploring folder")

		query := c.service.Files.
			List().
			PageSize(1000).
			Fields("nextPageToken, files(id, name, mimeType, lastModifyingUser(displayName))").
			Q(fmt.Sprintf("parents in \"%s\"", current.id))

		// Page through results
		for tok := "."; tok != ""; {
			result, err := query.Do()
			if err != nil {
				files <- err
				return
			}

			for _, f := range result.Files {
				if err := c.setFileFolder(f.Id, current.path); err != nil {
					files <- err
					return
				}

				switch f.MimeType {
				case MimeTypeDriveFolder:
					folders = append(folders, folder{
						id:   f.Id,
						path: filepath.Join(current.path, f.Name),
					})

				case MimeTypeDriveDoc:
					files <- File{
						Id:     f.Id,
						Name:   f.Name,
						Author: f.LastModifyingUser.DisplayName,
						Path:   current.path,
						// TODO: Publish time (maybe from content?)
					}

				default:
					log.WithFields(log.Fields{
						"site":   c.site.HostPathPrefix(),
						"folder": current,
						"type":   f.MimeType,
						"name":   f.Name,
					}).Warn("skipping drive object of unknown type")
				}
			}

			tok = result.NextPageToken
			query = query.PageToken(tok)
		}
	}
}

// Recursively list all Google Doc files nested under `folderId`.
// Implemented as breadth first search.
func (c Client) List(folderId string) <-chan FileResult {
	files := make(chan FileResult, 1)

	go c.listFolder(folderId, files)

	return files
}
