package main

import (
	"encoding/json"
	"io/ioutil"
	"strings"

	"github.com/BurntSushi/toml"
	log "github.com/sirupsen/logrus"

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

func init() {
	log.SetFormatter(&log.TextFormatter{})
}

func main() {
	conf := loadConfiguration()

	store, err := store.New(conf.Store)
	if err != nil {
		log.Fatalf("failed to create redis store: %v\n", err)
	}

	var site model.Site
	json.NewDecoder(strings.NewReader(conf.Site)).Decode(&site)

	provider := drive.NewGoogleDriveProvider(conf.Drive, store)
	client, err := provider.ClientForSite(site)
	if err != nil {
		log.Fatalf("failed to create drive client: %v\n", err)
	}

	w := web.New([]model.Site{site}, conf.Web, store)

	if err := w.RegisterDriveHooks(client); err != nil {
		log.Fatalf("failed to register drive hooks: %v\n", err)
	}

	go client.Start(true, conf.Drive)

	log.WithFields(log.Fields{
		"site": site.HostPathPrefix(),
	}).Info("Launching server")

	w.Serve()
}
