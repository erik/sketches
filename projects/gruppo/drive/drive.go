package drive

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	log "github.com/sirupsen/logrus"
	"golang.org/x/net/context"
	"golang.org/x/oauth2"
	"golang.org/x/oauth2/google"
	"google.golang.org/api/drive/v3"

	"github.com/erik/gruppo/converters"
	"github.com/erik/gruppo/model"
	"github.com/erik/gruppo/store"
	"github.com/erik/gruppo/util"
)

func NewGoogleDriveProvider(conf Configuration, db *store.RedisStore) GoogleDriveProvider {
	oauth := oauth2.Config{
		ClientID:     conf.ClientId,
		ClientSecret: conf.ClientSecret,
		RedirectURL:  conf.RedirectURI,

		Scopes:   []string{drive.DriveReadonlyScope},
		Endpoint: google.Endpoint,
	}

	return GoogleDriveProvider{oauth, conf, db}
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

	log.WithFields(log.Fields{
		"site":  c.site.HostPathPrefix(),
		"force": forceSync,
	}).Info("starting sync for site")

	root := c.site.Drive.FolderId
	if err := c.syncFolder(root, "", forceSync); err != nil {
		log.Fatal(err)
	}

	log.WithField("site", c.site.HostPathPrefix()).
		Info("finished sync for site")
}

func (c Client) exportFile(file File, dir string) (*model.Post, error) {
	docx, err := c.exportAsDocx(file)
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

	post.PublishDate = file.CreatedTime
	post.Author = file.Author
	post.Slug = post.GenerateSlug(file.Path, file.Name)

	if err := converters.HandlePostMedia(c.site, &post); err != nil {
		return nil, err
	}

	return &post, nil
}

func (c Client) processFile(file File, tmpDir string) (*model.Post, error) {
	post, err := c.exportFile(file, tmpDir)
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

	// FIXME: This is an ugly way to do this
	post.FullPath = strings.Replace(c.site.BasePath+"/"+slug, "//", "/", -1)
	post.Slug = slug

	if err := c.addOrUpdatePost(*post); err != nil {
		return nil, err
	}

	return post, nil
}

func (c Client) syncFolder(folderId, folderPath string, force bool) error {
	dir, err := ioutil.TempDir("/tmp", "exported-media-")
	if err != nil {
		log.Fatal(err)
	}

	defer os.RemoveAll(dir)

	if force {
		// Start with a clean slate
		if err := c.db.ClearSiteData(c.site); err != nil {
			return err
		}
	}

	// Generated List of posts
	overview := []model.PostOverview{}

	for res := range c.list(folderId, folderPath, force) {
		file, ok := res.(File)
		if !ok {
			return res.(error)
		}

		post, err := c.processFile(file, dir)
		if err != nil {
			return err
		}

		overview = append(overview, post.Overview())
	}

	// sort the post overviews by creation time
	sort.Slice(overview, func(i, j int) bool {
		return overview[j].PublishDate.Time.
			Before(overview[i].PublishDate.Time)
	})

	return c.db.SetPostOverviews(c.site, overview)
}

func (c Client) exportAsDocx(file File) (io.Reader, error) {
	// TODO: Support large file download via chunked transfer.
	res, err := c.service.Files.
		Export(file.Id, mimeTypeDocx).
		Download()

	if err != nil {
		return nil, err
	}

	return res.Body, nil
}

const (
	fileFields = "id, name, mimeType, createdTime, lastModifyingUser(displayName)"
)

func (c Client) getFileMeta(fileId string) (*drive.File, error) {
	return c.service.
		Files.
		Get(fileId).
		Fields(fileFields).
		Do()
}

type folder struct {
	path string
	id   string
}

func (c Client) listFolder(rootId, rootPath string, files chan FileResult, force bool) {
	defer close(files)

	var current folder

	// Ids of folders that we haven't explored yet
	folders := []folder{
		folder{id: rootId, path: rootPath},
	}

	for len(folders) > 0 {
		current, folders = folders[0], folders[1:]

		query := c.service.Files.
			List().
			PageSize(1000).
			Fields("nextPageToken", "files("+fileFields+")").
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
				case mimeTypeDriveFolder:
					folders = append(folders, folder{
						id:   f.Id,
						path: filepath.Join(current.path, f.Name),
					})

				case mimeTypeDriveDoc:
					files <- fileFromAPI(f, current.path)

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
func (c Client) list(folderId, folderPath string, force bool) <-chan FileResult {
	files := make(chan FileResult, 1)

	go c.listFolder(folderId, folderPath, files, force)

	return files
}

func fileFromAPI(f *drive.File, path string) File {
	ts, _ := time.Parse(time.RFC3339, f.CreatedTime)

	return File{
		Id:          f.Id,
		Name:        f.Name,
		Path:        path,
		CreatedTime: util.JSONTime{ts},
		Author:      f.LastModifyingUser.DisplayName,
	}
}
