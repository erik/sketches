package providers

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

type DriveConfiguration struct {
	ClientId     string
	ClientSecret string
	RedirectURI  string
}

type GoogleDriveProvider struct {
	oauth *oauth2.Config
}

type DriveClient struct {
	service *drive.Service
}

func NewGoogleDriveProvider(conf DriveConfiguration) GoogleDriveProvider {
	config := &oauth2.Config{
		ClientID:     conf.ClientId,
		ClientSecret: conf.ClientSecret,
		RedirectURL:  conf.RedirectURI,

		Scopes:   []string{drive.DriveReadonlyScope},
		Endpoint: google.Endpoint,
	}

	return GoogleDriveProvider{config}
}

func (p GoogleDriveProvider) ClientForToken(tok *oauth2.Token) (*DriveClient, error) {
	client := p.oauth.Client(context.TODO(), tok)

	svc, err := drive.New(client)
	if err != nil {
		return nil, err
	}

	return &DriveClient{svc}, nil

}

func (c DriveClient) ExportAsDocx(file DriveFile) (io.Reader, error) {
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

type driveFolder struct {
	path string
	id   string
}

type DriveFile struct {
	Id     string
	Name   string
	Path   string
	Author string
}

// Either `DriveFile` or an error. It's like a union type, but fucking stupid.
type DriveFileResult interface{}

func (c DriveClient) listFolder(rootId string, files chan DriveFileResult) {
	defer close(files)

	var currentFolder driveFolder

	// Ids of folders that we haven't explored yet
	folders := []driveFolder{driveFolder{
		id:   rootId,
		path: "",
	}}

	for len(folders) > 0 {
		currentFolder, folders = folders[0], folders[1:]

		log.Printf("[INFO] exploring folder: %v\n", currentFolder)

		query := c.service.Files.
			List().
			PageSize(1000).
			Fields("nextPageToken, files(id, name, mimeType, lastModifyingUser(displayName))").
			Q(fmt.Sprintf("parents in \"%s\"", currentFolder.id))

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
					folders = append(folders, driveFolder{
						id:   f.Id,
						path: filepath.Join(currentFolder.path, f.Name),
					})

				case MimeTypeDriveDoc:
					files <- DriveFile{
						Id:     f.Id,
						Name:   f.Name,
						Author: f.LastModifyingUser.DisplayName,
						Path:   currentFolder.path,
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
func (c DriveClient) List(folderId string) <-chan DriveFileResult {
	files := make(chan DriveFileResult, 1)

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
