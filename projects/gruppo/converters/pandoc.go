package converters

import (
	"bufio"
	"io/ioutil"
	"log"
	"os/exec"
	"regexp"
	"strings"

	"github.com/erik/gruppo/model"
)

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

// Handle pandoc generated markdown blocks, e.g.
//
// ---
// title: 'foobar'
// xyz: 'baz'
// ...
func extractMetadata(lines []string, post *model.Post) int {
	idx := 0

	for ; idx < len(lines); idx++ {
		line := lines[idx]

		// End of block, skip the line and move on to content
		if line == "..." {
			idx++
			break
		}

		kv := strings.SplitN(line, ": ", 2)
		if len(kv) < 2 {
			continue
		} else if kv[1] == "|" {
			kv[1] = ""

			// Roll up multiline values
			for idx++; lines[idx] == "" || strings.HasPrefix(lines[idx], " "); idx++ {
				kv[1] += strings.TrimSpace(lines[idx])
			}
		}

		switch kv[0] {
		case "title":
			post.Title = cleanTitle(kv[1])

		case "subtitle":
			post.Subtitle = cleanTitle(kv[1])
		}
	}

	return idx
}

// ExtractPost converts a markdown string into Post
func ExtractPost(markdown string) model.Post {
	var post model.Post

	lines := strings.Split(markdown, "\n")
	for i := range lines {
		lines[i] = stripAnchorSpans(lines[i])
	}

	// Where does the metadata end and content start?
	idx := 0

	// Grab and parse the metadata block, if it exists
	if lines[0] == "---" {
		idx = extractMetadata(lines, &post)
	} else {
		// If we don't have a real title, take the first non blank line.
		for _, line := range lines {
			if line != "" {
				post.Title = line
			}
		}
	}

	post.Content = strings.Join(lines[idx:], "\n")
	post.ImagePaths = extractImagePaths(post.Content)

	return post
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
