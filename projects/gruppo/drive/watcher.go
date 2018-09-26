// support for google drive's push notifications
package drive

import (
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"os"
	"time"

	"github.com/google/uuid"
	"google.golang.org/api/drive/v3"
)

func (c Client) ChangeHookRoute() string {
	key := c.site.WebhookKey()

	// TODO: This is idiotic.
	url, err := url.Parse(c.config.WatchNotificationURI + key)
	if err != nil {
		panic(err)
	}

	return url.Path
}

// Continuously listen for file changes in Redis, processing files as
// they appear. Applies a debounce so that multiple quick edits to the
// same file are not consuming the worker.
func (c Client) changeHandler() {
	for range time.Tick(5 * time.Second) {
		ch, err := c.popFileChange()
		if err != nil {
			log.Printf("Dequeue changes failed: %+v\n", err)
			return
		}

		// If we don't have any work, sleep a little less
		if ch == nil {
			continue
		}

		dir, err := ioutil.TempDir("/tmp", "exported-media-")
		if err != nil {
			log.Fatal(err)
		}

		defer os.RemoveAll(dir)

		post, err := c.ProcessFile(ch.File, dir)
		if err != nil {
			log.Printf("ProcessFile failed: %+v\n", err)
			return
		}

		log.Printf("Handled file change %+v\n", post)

		// Debounce
		time.Sleep(30 * time.Second)
	}
}

func (c Client) HandleChangeHook(req *http.Request) error {
	headers := req.Header

	resources := headers["X-Goog-Resource-Id"]
	if len(resources) < 1 {
		log.Printf("Hook missing resource id")
		return nil
	}

	resourceId := resources[0]

	state := headers["X-Goog-Resource-State"]
	if len(state) < 1 {
		log.Printf("Hook missing resource state")
		return nil
	}

	var changeKind string

	switch state[0] {
	case "sync":
		log.Printf("Hook registered successfully for %s\n", c.site.HostPathPrefix())
		return nil

	case "add":
		log.Printf("Adding resource: %+v\n", resourceId)
		changeKind = fileChangeCreated

	case "update", "change":
		log.Printf("Updating resource: %+v\n", resourceId)
		changeKind = fileChangeUpdated

	case "remove", "trash":
		log.Printf("Removing resource: %+v\n", resourceId)
		changeKind = fileChangeDeleted

	default:
		log.Printf("Unknown value for resource state: %+v\n", state)
		return nil
	}

	fileId, err := c.getResourceFile(resourceId)
	if err != nil {
		log.Printf("Failed to find fileId for resource: %s: %+v\n", resourceId, err)
		return err
	}

	path, err := c.getFileFolder(fileId)
	if err != nil {
		log.Printf("Failed to find folder for fileId: %s: %+v\n", fileId, err)
		return err
	}

	change := FileChange{
		Kind: changeKind,
		File: File{
			Id:   fileId,
			Path: path,
		},
	}

	return c.pushFileChange(change)
}

// Periodically recreate the webhook, since Drive's API only allows
// them to live briefly.
func (c Client) changeWatcherRefresher(fileId string) {
	key := c.site.WebhookKey()

	for range time.Tick(1 * time.Hour) {
		ch, err := c.createChangeWatcher(fileId, key)
		if err != nil {
			log.Printf("ERROR: Failed to register file watcher: %+v\n", err)
		}

		if err := c.setResourceFile(fileId, ch.ResourceId); err != nil {
			log.Printf("ERROR: Failed to store resourceId -> fileId mapping %+v\n", err)
		}
	}
}

func (c Client) createChangeWatcher(fileId string, key string) (*drive.Channel, error) {
	channel := &drive.Channel{
		Address: c.config.WatchNotificationURI + key,
		Id:      uuid.New().String(),
		Type:    "web_hook",
	}

	ch, err := c.service.Files.
		Watch(fileId, channel).
		Do()

	if err != nil {
		log.Printf("Failed to register file watcher: %+v\n", err)
		return nil, err
	}

	return ch, nil
}
