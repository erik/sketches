package converters

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	log "github.com/sirupsen/logrus"

	"github.com/erik/gruppo/model"
)

func HandlePostMedia(s *model.Site, p *model.Post) error {
	assetPath := filepath.Join(s.SiteDir, "assets", p.Slug)

	if err := os.MkdirAll(assetPath, os.ModePerm); err != nil {
		return err
	}

	for _, path := range p.ImagePaths {
		name := filepath.Base(path)

		reader, err := os.Open(path)
		if err != nil {
			log.WithError(err).Error("failed to open file")
			return err
		}

		writer, err := os.Create(filepath.Join(assetPath, name))
		if err != nil {
			log.WithError(err).Error("failed to create output file")
			return err
		}

		defer writer.Close()

		if err := resizeImage(reader, writer); err != nil {
			log.WithError(err).Error("failed to resize image")
			return err
		}

		newURI := filepath.Join(s.BasePath, s.AssetPath, p.Slug, name)

		log.WithFields(log.Fields{
			"site":      s.HostPathPrefix(),
			"post_slug": p.Slug,
			"from":      path,
			"to":        newURI,
		}).Debug("rewrote media location")

		p.Content = strings.Replace(p.Content, path, newURI, -1)
	}

	return nil
}

const (
	maxImageWidth  = 1000
	maxImageHeight = 1500
)

func resizeImage(r io.Reader, w io.Writer) error {
	maxSize := fmt.Sprintf("%dx%d>", maxImageWidth, maxImageHeight)

	args := []string{
		"-", // read from stdin
		"-resize", maxSize,
		"-quality", "90",
		"jpeg:-", // write to stdout as jpeg
	}

	cmd := exec.Command("convert", args...)

	stdin, err := cmd.StdinPipe()
	if err != nil {
		log.WithError(err).Error("failed to open `convert` stdin")
		return err
	}

	stdout, err := cmd.StdoutPipe()
	if err != nil {
		log.WithError(err).Error("failed to open `convert` stdout")
		return err
	}

	stderr, err := cmd.StderrPipe()
	if err != nil {
		log.WithError(err).Error("failed to open `convert` stderr")
		return err
	}
	defer stderr.Close()

	go func() {
		defer stdin.Close()
		if _, err := io.Copy(stdin, r); err != nil {
			log.WithError(err).Error("writing to stdin failed")
		}
	}()

	go func() {
		defer stdout.Close()
		if _, err := io.Copy(w, stdout); err != nil {
			log.WithError(err).Error("reading from stdout failed")
		}
	}()

	if err := cmd.Start(); err != nil {
		log.WithError(err).Error("failed to start `convert` process")
		return err
	}

	if err := cmd.Wait(); err != nil {
		output, _ := ioutil.ReadAll(stderr)
		log.WithError(err).
			WithField("stderr", output).
			Error("failed to await `convert` process")

		return err
	}

	return nil
}
