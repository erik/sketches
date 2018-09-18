package main

import (
	"encoding/json"
	"io/ioutil"
	"log"
	"strings"

	"github.com/BurntSushi/toml"

	"github.com/erik/gruppo/drive"
	"github.com/erik/gruppo/model"
	"github.com/erik/gruppo/store"
	"github.com/erik/gruppo/web"
)

type Configuration struct {
	Drive drive.Configuration
	Web   web.Configuration
	Store store.Configuration

	// Temporary, eventually should be in redis
	Site string
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

func main() {
	conf := loadConfiguration()
	log.Printf("conf, %+v", conf)

	store, err := store.New(conf.Store)
	if err != nil {
		log.Fatal(err)
	}

	var site model.Site
	json.NewDecoder(strings.NewReader(conf.Site)).Decode(&site)

	provider := drive.NewGoogleDriveProvider(conf.Drive, store)
	client, err := provider.ClientForToken(site.Drive.Token)
	if err != nil {
		log.Fatal(err)
	}

	log.Printf("[INFO] starting sync for site: %s", site.HostPathPrefix())

	w := web.New([]model.Site{site}, conf.Web, store)

	if err := w.RegisterDriveHooks(&site, client); err != nil {
		log.Fatal(err)
	}

	go client.Start(true, &site, conf.Drive)

	w.Serve()
}
