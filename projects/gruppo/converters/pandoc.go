package converters

import (
	"log"
	"os/exec"
	"strings"
)

type PostData struct {
	Meta struct {
		Title    string
		Subtitle string
		Author   string
		Date     string
	}

	Content string
}

// Suckily, pandoc can't read .docx from streams, so we have to use a file
// interface.
func ConvertDocx(inputFile string, outputFile string, mediaDir string) error {
	args := []string{
		// from docx
		"-f", "docx",
		// to markdown
		"-t", "markdown",
		// standalone file (including meta)
		"-s",
		"--extract-media", mediaDir,
		"-i", inputFile,
		"-o", outputFile,
	}

	cmd := exec.Command("pandoc", args...)

	out, err := cmd.CombinedOutput()
	if err != nil {
		log.Printf("failed to open pandoc output: %v\n", err)
		return err
	}

	// Show pandoc output
	if len(out) > 0 {
		for _, line := range strings.Split(string(out), "\n") {
			log.Println(line)
		}
	}

	return nil
}
