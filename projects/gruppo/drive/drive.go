package drive

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"time"

	"github.com/erik/gruppo/converters"
	"github.com/erik/gruppo/model"
	"github.com/erik/gruppo/store"

	"github.com/google/uuid"
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
}

type Client struct {
	service *drive.Service
	config  *Configuration
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

type FileChange struct {
	file File
	site model.Site
	kind string // one of "created", "deleted", "updated"
}

// Either `File` or an error. It's like a union type, but fucking stupid.
type FileResult interface{}

func NewGoogleDriveProvider(conf Configuration) GoogleDriveProvider {
	oauth := &oauth2.Config{
		ClientID:     conf.ClientId,
		ClientSecret: conf.ClientSecret,
		RedirectURL:  conf.RedirectURI,

		Scopes:   []string{drive.DriveReadonlyScope},
		Endpoint: google.Endpoint,
	}

	return GoogleDriveProvider{oauth, &conf}
}

func (p GoogleDriveProvider) ClientForToken(tok *oauth2.Token) (*Client, error) {
	client := p.oauth.Client(context.TODO(), tok)

	svc, err := drive.New(client)
	if err != nil {
		return nil, err
	}

	return &Client{svc, p.config}, nil
}

func (c Client) Start(forceSync bool, site *model.Site, store *store.RedisStore, conf Configuration) {
	go c.changeWatcherRefresher(site.Drive.FolderId, site, store)

	if forceSync {
		if err := c.ForceSync(*site, store); err != nil {
			log.Fatal(err)
		}
	}
}

func (c Client) ChangeHookRoute(site *model.Site) string {
	key := site.WebhookKey()

	// TODO: This is idiotic.
	url, err := url.Parse(c.config.WatchNotificationURI + key)
	if err != nil {
		panic(err)
	}

	return url.Path
}

func (c Client) HandleChangeHook(site *model.Site, req *http.Request, db *store.RedisStore) error {
	headers := req.Header

	resource := headers["X-Goog-Resource-ID"]
	if len(resource) < 1 {
		log.Printf("Hook missing resource id")
		return nil
	}

	state := headers["X-Goog-Resource-State"]
	if len(state) < 1 {
		log.Printf("Hook missing resource state")
		return nil
	}

	switch state[0] {
	case "add":
		log.Printf("Adding resource: %+v\n", resource)

	case "update", "change":
		log.Printf("Updating resource: %+v\n", resource)

	case "remove", "trash":
		log.Printf("Removing resource: %+v\n", resource)

	default:
		log.Printf("Unknown value for resource state: %+v\n", state)
		return nil
	}

	return nil
}

func (c Client) changeWatcherRefresher(fileId string, site *model.Site, store *store.RedisStore) {
	key := site.WebhookKey()

	_, err := c.CreateChangeWatcher(fileId, key)
	if err != nil {
		log.Printf("ERROR: Failed to register file watcher: %+v\n", err)
	}

	// Watchers expire very frequently
	time.Sleep(55 * time.Minute)

}

func (c Client) CreateChangeWatcher(fileId string, key string) (string, error) {
	id := uuid.New().String()

	channel := &drive.Channel{
		Address: c.config.WatchNotificationURI + key,
		Id:      id,
		Type:    "web_hook",
	}

	_, err := c.service.Files.
		Watch(fileId, channel).
		Do()

	if err != nil {
		log.Printf("Failed to register file watcher: %+v\n", err)
		return "", err
	}

	return id, nil
}

func (c Client) ExportFile(file File, site model.Site, dir string) (*model.Post, error) {
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

	if err := converters.HandlePostMedia(&site, &post); err != nil {
		return nil, err
	}

	return &post, nil
}

// ForceSync ...
func (c Client) ForceSync(site model.Site, db *store.RedisStore) error {
	dir, err := ioutil.TempDir("/tmp", "exported-media-")
	if err != nil {
		log.Fatal(err)
	}

	defer os.RemoveAll(dir)

	// Start with a clean slate
	if err := db.ClearSiteData(site); err != nil {
		return err
	}

	// Generated List of posts
	overview := []model.PostOverview{}

	for res := range c.List(site.Drive.FolderId) {
		file, ok := res.(File)
		if !ok {
			return res.(error)
		}

		post, err := c.ExportFile(file, site, dir)
		if err != nil {
			log.Printf("Failed to export file from drive: %+v\n", err)
			return err
		}

		go c.changeWatcherRefresher(file.Id, &site, db)

		overview = append(overview, post.Overview())

		log.Printf("[INFO] extracted post: %+v", post.Slug)

		if err := db.AddPost(site, *post); err != nil {
			return err
		}
	}

	if err := db.SetPostOverviews(site, overview); err != nil {
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
