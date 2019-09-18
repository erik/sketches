package main

import (
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"os"
	"path"
	"strings"
)

// cli flags
var (
	uploadDir string
	baseUrl   string
	port      int
	host      string
)

func init() {
	flag.StringVar(&uploadDir, "uploaddir", "/var/www/oop/",
		"where to keep uploaded files")
	flag.StringVar(&baseUrl, "url", "",
		"base url content is served from")
	flag.StringVar(&host, "host", "", "host to listen on")
	flag.IntVar(&port, "port", 8080, "port to listen on")

	flag.Parse()

	if baseUrl == "" {
		log.Fatal("-url is required")
	}
}

func main() {
	http.HandleFunc("/delete", handleDelete)
	http.HandleFunc("/up", handleUpload)
	http.HandleFunc("/", func(w http.ResponseWriter, req *http.Request) {
		if req.URL.Path != "/" {
			log.Printf("not found: %s", req.URL.Path)
			http.NotFound(w, req)
			return
		}

		handleIndex(w, req)
	})

	addr := fmt.Sprintf("%s:%d", host, port)

	log.Printf("starting server on %s", addr)
	http.ListenAndServe(addr, nil)
}

func handleUpload(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "not allowed", http.StatusMethodNotAllowed)
		return
	}

	file, header, err := r.FormFile("file")
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	defer file.Close()

	s := strings.Split(header.Filename, ".")
	name, ext := s[0], s[len(s)-1]

	tmpfile, err := ioutil.TempFile(uploadDir, fmt.Sprintf("*.%s", ext))
	if err != nil {
		log.Printf("create temp file: %+v", err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	defer tmpfile.Close()

	log.Printf("receiving file: %s.%s => %s", name, ext, tmpfile.Name())

	if b, err := io.Copy(tmpfile, file); err != nil {
		log.Printf("download failed: %+v", err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	} else {
		log.Printf("%s completed (%d bytes)", tmpfile.Name(), b)
	}

	// Tempfiles are created as 0600, which means other processes
	// won't be able to read them.
	if err := os.Chmod(tmpfile.Name(), 0644); err != nil {
		log.Printf("chmod failed: %+v", err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	url, _ := url.Parse(baseUrl)
	url.Path = path.Join(url.Path, path.Base(tmpfile.Name()))
	w.Write([]byte(url.String()))
}

func handleDelete(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "not allowed", http.StatusMethodNotAllowed)
		return
	}

	panic("not implemented")
}

func handleIndex(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "not allowed", http.StatusMethodNotAllowed)
		return
	}

	w.Header().Add("Content-Type", "text/html")
	w.Write([]byte(`
<!doctype html>
<head>
  <title>oop</title>
  <meta name="viewport"
        content="width=device-width, initial-scale=1">
</head>

<style>
  body { padding: 0 1em; }
  input { margin: 1em 0; }
</style>

<h1>oop</h1>
<form method="post" action="up" enctype="multipart/form-data">
  <input type="file" name="file"> <br/>
  <input type="submit" value="Upload">
</form>
`))
}
