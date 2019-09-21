package main

import (
	"flag"
	"fmt"
	"html/template"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"os"
	"path"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// cli flags
var (
	uploadDir string
	baseUrl   string
	port      int
	host      string
)

func init() {
	flag.StringVar(&uploadDir, "uploaddir", "/var/www/oop/", "where to keep uploaded files")
	flag.StringVar(&baseUrl, "url", "", "base url content is served from")
	flag.StringVar(&host, "host", "", "host to listen on")
	flag.IntVar(&port, "port", 8080, "port to listen on")

	flag.Parse()

	if baseUrl == "" {
		log.Fatal("-url is required")
	}

	// uploadDir needs to end with a trailing slash for filepath.Walk
	uploadDir = path.Join(uploadDir, "/")
}

func main() {
	http.HandleFunc("/up", logRequest(handleUpload))
	http.HandleFunc("/", logRequest(handleIndex))

	addr := fmt.Sprintf("%s:%d", host, port)

	log.Printf("starting server on %s", addr)
	http.ListenAndServe(addr, nil)
}

func logRequest(fn http.HandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		s := time.Now()
		fn(w, r)
		dt := (time.Now().UnixNano() - s.UnixNano()) / int64(time.Millisecond)

		log.Printf("%s %s %s (%d ms)", r.RemoteAddr, r.Method, r.URL.Path, dt)
	}
}

func internalServerError(msg string, err error, w http.ResponseWriter) {
	log.Printf("%s %+v", msg, err)
	http.Error(w, "internal server error", http.StatusInternalServerError)
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
		internalServerError("create temp file", err, w)
		return
	}
	defer tmpfile.Close()

	log.Printf("receiving file: %s.%s => %s", name, ext, tmpfile.Name())

	bytes, err := io.Copy(tmpfile, file)
	if err != nil {
		internalServerError("download from client", err, w)
		return
	}
	log.Printf("%s completed (%d bytes)", tmpfile.Name(), bytes)

	// Tempfiles are created as 0600, which means other processes
	// won't be able to read them.
	if err := os.Chmod(tmpfile.Name(), 0644); err != nil {
		internalServerError("chmod", err, w)
		return
	}

	w.Write([]byte(staticUrl(tmpfile.Name()) + "\n"))
}

// staticUrl takes a file system path (presumably within the upload
// directory, but it doesn't matter) and returns the URL that this file
// is served at.
func staticUrl(filePath string) string {
	url, _ := url.Parse(baseUrl)
	url.Path = path.Join(url.Path, path.Base(filePath))
	return url.String()
}

type staticFile struct {
	Timestamp string
	Name      string
	Url       string
	SizeKb    int64
}

// listFiles returns all files inside `uploadDir`.
func listFiles(dir string) ([]staticFile, error) {
	fileInfo := []os.FileInfo{}
	err := filepath.Walk(dir, func(_ string, info os.FileInfo, err error) error {
		if err != nil || info.IsDir() {
			return err
		}

		fileInfo = append(fileInfo, info)
		return nil
	})

	// newest files first
	sort.Slice(fileInfo, func(i, j int) bool {
		return fileInfo[i].ModTime().Unix() > fileInfo[j].ModTime().Unix()
	})

	files := make([]staticFile, len(fileInfo))
	for i, info := range fileInfo {
		files[i] = staticFile{
			Timestamp: info.ModTime().Format("2006-01-02 15:04"),
			Name:      info.Name(),
			Url:       staticUrl(info.Name()),
			SizeKb:    info.Size() / 1024,
		}
	}
	return files, err
}

var indexTemplate = template.Must(template.New("index").Parse(`
<!doctype html>
<head>
  <title>oop</title>
  <meta name="viewport" content="width=device-width, initial-scale=1">
</head>

<style>
  * { font-family: sans-serif; }
</style>

<h1>oop</h1>
<form method="post" action="up" enctype="multipart/form-data">
  <input type="file" name="file"> <br/>
  <input type="submit" value="Upload">
</form>

<h1>uploads</h1>
<ul>
  {{range .}}
    <li>
      <em>{{.Timestamp}}</em>
      <a href="{{.Url}}">{{ .Name }}</a>
      ({{.SizeKb}} KiB)
    </li>
  {{end}}
</ul>
`))

func handleIndex(w http.ResponseWriter, r *http.Request) {
	// Go will treat `/` as a catch-all wildcard route.
	if r.URL.Path != "/" {
		log.Printf("not found: %s", r.URL.Path)
		http.NotFound(w, r)
		return
	} else if r.Method != http.MethodGet {
		http.Error(w, "not allowed", http.StatusMethodNotAllowed)
		return
	}

	files, err := listFiles(uploadDir)
	if err != nil {
		internalServerError("list static files", err, w)
		return
	}

	w.Header().Add("Content-Type", "text/html")
	if err := indexTemplate.Execute(w, files); err != nil {
		internalServerError("render template", err, w)
	}
}
