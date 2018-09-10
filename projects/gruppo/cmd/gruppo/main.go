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

	provider := providers.NewGoogleDriveProvider("secrets/credentials.json")
	files, _ := provider.List(directory)

	tempDir, err := ioutil.TempDir("/tmp", "exported-media")
	if err != nil {
		log.Fatal(err)
	}

	defer os.RemoveAll(tempDir)
	log.Printf("temp directory is: %v\n", tempDir)

	for f := range files {
		fmt.Printf("File: %s '%s/%s' (author=%s) \n", f.Id, f.Path, f.Name, f.Author)
		docx, err := provider.ExportAsDocx(f)

		if err != nil {
			log.Fatal(err)
		}

		path := filepath.Join(tempDir, f.Id)
		if err := os.Mkdir(path, os.ModePerm); err != nil {
			log.Fatal(err)
		}

		inputFileName := filepath.Join(path, "input.docx")
		inputFile, err := os.Create(inputFileName)
		if err != nil {
			log.Fatal(err)
		}

		writer := bufio.NewWriter(inputFile)
		io.Copy(writer, docx)
		writer.Flush()

		md, err := converters.ConvertDocx(inputFileName, path)
		if err != nil {
			log.Fatal(err)
		}

		postData := converters.ExtractPostData(md)
		log.Printf("postdata => %+v", postData)

		writer.Flush()
	}

	log.Printf("Hit enter to clean up")

	var ignored string
	fmt.Scanln(&ignored)
}
