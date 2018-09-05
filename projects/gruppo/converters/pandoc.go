package converters

import (
	"log"
	"os/exec"
)

var PandocArgs []string = []string{
	"-f", "docx", // from docx
	"-t", "markdown", // to markdown
	"-s", // standalone
}

// Suckily, pandoc can't read .docx from streams, so we have to use a file
// interface.
func ConvertDocx(inputFile string, outputFile string, mediaDir string) error {
	args := append(PandocArgs,
		"--extract-media", mediaDir,
		"-i", inputFile,
		"-o", outputFile,
	)
	cmd := exec.Command("pandoc", args...)

	out, err := cmd.CombinedOutput()
	if err != nil {
		log.Printf("failed to open pandoc output: %v\n", err)
		return err
	}

	log.Printf("pandoc: %s\n", out)

	return nil
}
