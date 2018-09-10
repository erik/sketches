package converters

import (
	"bufio"
	"io/ioutil"
	"log"
	"os/exec"
	"regexp"
	"strings"
)

type PostData struct {
	Meta struct {
		Title    string
		Subtitle string
		Author   string
		Date     string
	}

	Content    string
	ImagePaths []string
}

// <span id="_cswwk72is3z0" class="anchor"></span>Title => Title
func stripAnchorSpans(in string) string {
	re := regexp.MustCompile("<span [^>]+?></span>(.*)")
	return re.ReplaceAllString(in, "${1}")
}

// 'foo' => foo
func cleanTitle(in string) string {
	re := regexp.MustCompile("^'?(.*?)'?$")
	return re.ReplaceAllString(in, "${1}")
}

func extractImagePaths(content string) []string {
	re := regexp.MustCompile(`!\[.*?\]\((.*?)\)`)

	matches := []string{}

	for _, match := range re.FindAllStringSubmatch(content, -1) {
		matches = append(matches, match[1])
	}

	return matches
}

// ExtractPostData converts a markdown string into PostData
func ExtractPostData(markdown string) PostData {
	var postData PostData

	lines := strings.Split(markdown, "\n")
	for i := range lines {
		lines[i] = stripAnchorSpans(lines[i])
	}

	// Where does the metadata end and content start?
	idx := 0

	// Grab and parse the metadata block, if it exists
	if lines[0] == "---" {
		for ; idx < len(lines); idx += 1 {
			line := lines[idx]

			// End of block
			if line == "..." {
				break
			}

			kv := strings.SplitN(line, ": ", 2)
			if len(kv) < 2 {
				continue
			} else if kv[1] == "|" {
				kv[1] = ""

				// Roll up multiline values
				for idx += 1; lines[idx] == "" || strings.HasPrefix(lines[idx], " "); idx += 1 {
					kv[1] += strings.TrimSpace(lines[idx])
				}
			}

			switch kv[0] {
			case "title":
				postData.Meta.Title = cleanTitle(kv[1])

			case "subtitle":
				postData.Meta.Subtitle = cleanTitle(kv[1])
			}
		}
	} else {
		// If we don't have a real title, take the first non blank line.
		for _, line := range lines {
			if line != "" {
				postData.Meta.Title = line
			}
		}
	}

	postData.Content = strings.Join(lines[idx:], "\n")
	postData.ImagePaths = extractImagePaths(postData.Content)

	return postData
}

// Suckily, pandoc can't read .docx from streams, so we have to use a file
// interface.
func ConvertDocx(inputFile string, mediaDir string) (string, error) {
	args := []string{
		// from docx
		"-f", "docx",
		// to markdown
		"-t", "markdown",
		// standalone file (including meta)
		"-s",
		"--extract-media", mediaDir,
		"-i", inputFile,
	}

	cmd := exec.Command("pandoc", args...)

	stdout, err := cmd.StdoutPipe()
	if err != nil {
		log.Printf("failed to open pandoc stdout: %v\n", err)
		return "", err
	}

	stderr, err := cmd.StderrPipe()
	if err != nil {
		log.Printf("failed to open pandoc stderr: %v\n", err)
		return "", err
	}

	if err := cmd.Start(); err != nil {
		log.Printf("failed to run pandoc: %v\n", err)
		return "", err
	}

	markdown, err := ioutil.ReadAll(stdout)
	if err != nil {
		log.Printf("failed reading from pandoc")
		return "", err
	}

	if err := cmd.Wait(); err != nil {
		log.Printf("pandoc exited with an error: %v\n", err)

		// Make sure we print out the error output
		scanner := bufio.NewScanner(stderr)

		for scanner.Scan() {
			log.Println(scanner.Text())
		}

		return "", err
	}

	return string(markdown), nil
}
