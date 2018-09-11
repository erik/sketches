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
	"github.com/erik/gruppo/drive"
	"github.com/erik/gruppo/render"
)

type Configuration struct {
	Drive  drive.Configuration
	Server struct {
		Host         string
		Port         int
		TemplatePath string
	}

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

func main() {
	if len(os.Args) != 2 {
		log.Fatal("usage: gruppo [dir]")
	}

	conf := loadConfiguration()

	directory := os.Args[1]

	provider := drive.NewGoogleDriveProvider(conf.Drive)

	tok, err := drive.TokenFromFile("secrets/token.json")
	if err != nil {
		log.Fatal(err)
	}

	client, err := provider.ClientForToken(tok)
	if err != nil {
		log.Fatal(err)
	}

	tempDir, err := ioutil.TempDir("/tmp", "exported-media-")
	if err != nil {
		log.Fatal(err)
	}

	defer os.RemoveAll(tempDir)
	log.Printf("temp directory is: %v\n", tempDir)

	for r := range client.List(directory) {
		f, ok := r.(drive.File)
		if !ok {
			log.Fatalf("something went wrong listing directory: %+v", r)
		}

		fmt.Printf("File: %s '%s/%s' (author=%s) \n", f.Id, f.Path, f.Name, f.Author)

		docx, err := client.ExportAsDocx(f)
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
		postData.Author = f.Author

		log.Printf("postdata => %+v", postData)

		r, err := render.Render("post", "vanilla", &render.Context{
			Title: "Great Title",
			Post:  postData,
		})

		if err != nil {
			log.Fatal(err)
		}

		log.Printf("rendered => %s", r)
	}

	log.Printf("Hit enter to clean up")

	var ignored string
	fmt.Scanln(&ignored)
}
