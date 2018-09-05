package main

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"

	"github.com/BurntSushi/toml"

	"github.com/erik/gruppo/converters"
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
		reader, err := provider.ExportAsDocx(f)

		if err != nil {
			log.Fatal(err)
		}

		tempDir, err := ioutil.TempDir("/tmp", "exported-media")
		if err != nil {
			log.Fatal(err)
		}

		defer os.RemoveAll(tempDir)
		log.Printf("temp directory is: %v\n", tempDir)

		baseFile := filepath.Join(tempDir, f.Name)

		inputFile, err := os.Create(baseFile + ".docx")
		if err != nil {
			log.Fatal(err)
		}

		writer := bufio.NewWriter(inputFile)
		io.Copy(writer, reader)
		writer.Flush()

		if err := converters.ConvertDocx(baseFile+".docx", baseFile+".md", tempDir); err != nil {
			log.Fatal(err)
		}

		writer.Flush()
	}
}
