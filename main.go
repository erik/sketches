package main

import (
	"fmt"
	"io/ioutil"
	"log"

	"github.com/BurntSushi/toml"

	"github.com/erik/gruppo/providers"
)

type Configuration struct {
	Drive struct {
		ClientId     string
		ClientSecret string
	}
}

func readConfiguration() Configuration {
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
	config := readConfiguration()
	provider := providers.NewGoogleDriveProvider("secrets/credentials.json")

	fmt.Printf("The config value is: %v\n", config)
	fmt.Printf("This provider is: %v\n", provider)
}
