package drive

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"

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
	ClientId     string
	ClientSecret string
	RedirectURI  string
}

type GoogleDriveProvider struct {
	oauth *oauth2.Config
}

type Client struct {
	service *drive.Service
}

func NewGoogleDriveProvider(conf Configuration) GoogleDriveProvider {
	config := &oauth2.Config{
		ClientID:     conf.ClientId,
		ClientSecret: conf.ClientSecret,
		RedirectURL:  conf.RedirectURI,

		Scopes:   []string{drive.DriveReadonlyScope},
		Endpoint: google.Endpoint,
	}

	return GoogleDriveProvider{config}
}

func (p GoogleDriveProvider) ClientForToken(tok *oauth2.Token) (*Client, error) {
	client := p.oauth.Client(context.TODO(), tok)

	svc, err := drive.New(client)
	if err != nil {
		return nil, err
	}

	return &Client{svc}, nil

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

// Either `File` or an error. It's like a union type, but fucking stupid.
type FileResult interface{}

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

	tok, err := config.Exchange(oauth2.NoContext, authCode)
	if err != nil {
		log.Fatalf("Unable to retrieve token from web %v", err)
	}

	return tok
}

// Retrieves a token from a local file.
// TODO: Remove this, store token somewhere sensible
func TokenFromFile(file string) (*oauth2.Token, error) {
	f, err := os.Open(file)
	defer f.Close()
	if err != nil {
		return nil, err
	}

	tok := &oauth2.Token{}
	err = json.NewDecoder(f).Decode(tok)

	return tok, err
}

// Saves a token to a file path.
func saveToken(path string, token *oauth2.Token) {
	fmt.Printf("Saving credential file to: %s\n", path)
	f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0600)
	defer f.Close()

	if err != nil {
		log.Fatalf("Unable to cache oauth token: %v", err)
	}

	json.NewEncoder(f).Encode(token)
}
