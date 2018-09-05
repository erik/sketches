package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"

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
	if len(os.Args) != 2 {
		log.Fatal("usage: gruppo [dir]")
	}

	directory := os.Args[1]

	config := readConfiguration()
	provider := providers.NewGoogleDriveProvider("secrets/credentials.json")

	fmt.Printf("The config value is: %v\n", config)
	fmt.Printf("This provider is: %v\n", provider)

	files, _ := provider.List(directory)

	for f := range files {
		fmt.Printf("File: %s %s \n", f.Id, f.Name)
	}
}
