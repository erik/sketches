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

func syncSite(s model.Site, store store.RedisStore, conf Configuration) {
	provider := drive.NewGoogleDriveProvider(conf.Drive)

	client, err := provider.ClientForToken(s.Drive.Token)
	if err != nil {
		log.Fatal(err)
	}

	if err := client.ForceSync(s, store); err != nil {
		log.Fatal(err)
	}

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

	log.Printf("[INFO] starting sync for site: %s", site.HostPathPrefix())
	go syncSite(site, *store, conf)

	web.New([]model.Site{site}, conf.Web, store).Serve()
}
