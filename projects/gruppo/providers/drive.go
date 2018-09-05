package providers

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"

	"golang.org/x/net/context"
	"golang.org/x/oauth2"
	"golang.org/x/oauth2/google"
	"google.golang.org/api/drive/v3"
)

type GoogleDriveProvider struct {
	token   *oauth2.Token
	service *drive.Service
}

const (
	MIME_TYPE_DRIVE_FOLDER = "application/vnd.google-apps.folder"
	MIME_TYPE_DRIVE_DOC    = "application/vnd.google-apps.document"
)

func NewGoogleDriveProvider(filePath string) GoogleDriveProvider {
	config, err := readConfigFile(filePath)
	if err != nil {
		log.Fatalf("Failed to read config file: %v", err)
	}

	client := getClient(config)

	service, err := drive.New(client)
	if err != nil {
		log.Fatalf("Could not retrieve Drive client: %v", err)
	}

	provider := GoogleDriveProvider{
		service: service,
	}

	return provider
}

// Recursively list all Google Doc files nested under `folderId`. Implemented as breadth first search.
func (p GoogleDriveProvider) List(folderId string) (<-chan string, <-chan error) {
	files := make(chan string, 1)
	errors := make(chan error, 1)

	go func() {
		defer close(files)
		defer close(errors)

		var currentFolder string = ""

		// Ids of folders that we haven't explored yet
		folders := []string{folderId}

		for len(folders) > 0 {
			currentFolder, folders = folders[0], folders[1:]
			log.Printf("[INFO] exploring folder: %s\n", currentFolder)

			pageToken := ""

			query := p.service.Files.
				List().
				PageSize(1000).
				Fields("nextPageToken, files(id, name, mimeType)").
				Q(fmt.Sprintf("parents in \"%s\"", currentFolder))

			for {
				result, err := query.Do()
				if err != nil {
					errors <- err
					return
				}

				for _, f := range result.Files {
					switch f.MimeType {
					case MIME_TYPE_DRIVE_FOLDER:
						log.Printf("[INFO] queuing directory: %s\n", f.Name)
						folders = append(folders, f.Id)

					case MIME_TYPE_DRIVE_DOC:
						files <- f.Name

					default:
						log.Printf("[INFO] skipping object of unknown type (%s): %v\n", f.MimeType, f)
					}
				}

				if pageToken = result.NextPageToken; pageToken == "" {
					break
				}

				query = query.PageToken(pageToken)
			}
		}
	}()

	return files, errors
}

func readConfigFile(filePath string) (*oauth2.Config, error) {
	data, err := ioutil.ReadFile(filePath)

	if err != nil {
		return nil, err
	}

	config, err := google.ConfigFromJSON(data, drive.DriveReadonlyScope)
	if err != nil {
		return nil, err
	}

	return config, nil
}

// Retrieve a token, saves the token, then returns the generated client.
func getClient(config *oauth2.Config) *http.Client {
	tokenFile := "secrets/token.json"
	tok, err := tokenFromFile(tokenFile)

	if err != nil {
		tok = getTokenFromWeb(config)
		saveToken(tokenFile, tok)
	}

	return config.Client(context.Background(), tok)
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
func tokenFromFile(file string) (*oauth2.Token, error) {
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
