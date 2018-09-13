package main

import (
	"io/ioutil"
	"log"
	"os"

	"github.com/BurntSushi/toml"

	"github.com/erik/gruppo/drive"
	"github.com/erik/gruppo/store"
	"github.com/erik/gruppo/web"
)

type Configuration struct {
	Drive drive.Configuration
	Web   web.Configuration
	Store store.Configuration

	Sites struct{}
}

func loadConfiguration() Configuration {
	var conf Configuration

	data, err := ioutil.ReadFile("secrets/config.toml")
	if err != nil {
		log.Fatalf("Failed to read config file: %v", err)
	}

	if _, err := toml.Decode(string(data), &conf); err != nil {
		log.Fatalf("Failed to decode config file: %v", err)
	}

	return conf
}

func syncDrive(folderId string, store store.RedisStore, conf Configuration) {
	provider := drive.NewGoogleDriveProvider(conf.Drive)

	// TODO: Pull this out, redis or something?
	tok, err := drive.TokenFromFile("secrets/token.json")
	if err != nil {
		log.Fatal(err)
	}

	client, err := provider.ClientForToken(tok)
	if err != nil {
		log.Fatal(err)
	}

	if err := client.ForceSync(folderId, store); err != nil {
		log.Fatal(err)
	}

}

func main() {
	if len(os.Args) != 2 {
		log.Fatal("usage: gruppo [dir]")
	}

	conf := loadConfiguration()
	log.Printf("conf, %+v", conf)

	folderId := os.Args[1]

	store, err := store.New(conf.Store)
	if err != nil {
		log.Fatal(err)
	}

	go syncDrive(folderId, *store, conf)

	web.New(conf.Web).Serve()
}
