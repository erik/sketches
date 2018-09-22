package drive

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"

	"github.com/erik/gruppo/converters"
	"github.com/erik/gruppo/model"
	"github.com/erik/gruppo/store"

	"golang.org/x/net/context"
	"golang.org/x/oauth2"
	"golang.org/x/oauth2/google"
	"google.golang.org/api/drive/v3"
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
		log.Printf("Starting file sync for site %s\n", c.site.HostPathPrefix())

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

func (c Client) ProcessFile(file File, isNew bool, tmpDir string) (*model.Post, error) {
	post, err := c.ExportFile(file, tmpDir)
	if err != nil {
		log.Printf("Failed to export file from drive: %+v\n", err)
		return nil, err
	}

	go c.changeWatcherRefresher(file.Id)

	log.Printf("[INFO] extracted post: %+v", post.Slug)

	if isNew {
		if err := c.setSlugForFileId(file.Id, post.Slug); err != nil {
			return nil, err
		}
	} else {
		// If we're refreshing an existing post, don't change the slug
		slug, err := c.getSlugForFileId(file.Id)
		if err != nil {
			return nil, err
		}

		post.Slug = slug
	}

	if err := c.db.AddPost(c.site, *post); err != nil {
		return nil, err
	}

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

		post, err := c.ProcessFile(file, true, dir)
		if err != nil {
			return err
		}

		overview = append(overview, post.Overview())
	}

	if err := c.db.SetPostOverviews(c.site, overview); err != nil {
		return err
	}

	return nil
}

func (c Client) ExportAsDocx(file File) (io.Reader, error) {
	log.Printf("[INFO] exporting %s as .docx", file.Name)

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

		log.Printf("[INFO] exploring folder: %v\n", current)

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
					log.Printf("[INFO] queuing directory: %s\n", f.Name)
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
					log.Printf("[INFO] skipping object of unknown type (%s): %v\n", f.MimeType, f)
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

// Request a token from the web, then returns the retrieved token.
func getTokenFromWeb(config *oauth2.Config) *oauth2.Token {
	authURL := config.AuthCodeURL("state-token", oauth2.AccessTypeOffline)
	fmt.Printf("Go to the following link in your browser then type the "+
		"authorization code: \n%v\n", authURL)

	var authCode string
	if _, err := fmt.Scan(&authCode); err != nil {
		log.Fatalf("Unable to read authorization code %v", err)
	}

	tok, err := config.Exchange(context.Background(), authCode)
	if err != nil {
		log.Fatalf("Unable to retrieve token from web %v", err)
	}

	return tok
}

// Retrieves a token from a local file.
// TODO: Remove this, store token somewhere sensible
func TokenFromFile(file string) (*oauth2.Token, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}

	defer f.Close()

	tok := &oauth2.Token{}
	err = json.NewDecoder(f).Decode(tok)

	return tok, err
}

// Saves a token to a file path.
func saveToken(path string, token *oauth2.Token) {
	fmt.Printf("Saving credential file to: %s\n", path)
	f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0600)

	if err != nil {
		log.Fatalf("Unable to cache oauth token: %v", err)
	}

	defer f.Close()

	json.NewEncoder(f).Encode(token)
}
