package providers

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"io/ioutil"

	"golang.org/x/net/context"
	"golang.org/x/oauth2"
	"golang.org/x/oauth2/google"

	"google.golang.org/api/drive/v3"
)

type GoogleDriveProvider struct {
	token   *oauth2.Token
	service *drive.Service
}

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
