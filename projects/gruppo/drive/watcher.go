// support for google drive's push notifications
package drive

import (
	"encoding/json"
	"log"
	"net/http"
	"net/url"
	"strings"
	"time"

	"github.com/erik/gruppo/store"

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

func (c Client) enqueueFileChange(fc FileChange) error {
	k := store.KeyForSite(c.site, "changes")
	return c.db.AddSetJSON(k, fc)
}

func (c Client) dequeueFileChanges() ([]FileChange, error) {
	k := store.KeyForSite(c.site, "changes")
	items, err := c.db.PopSetMembers(k)

	if err != nil {
		return nil, err
	}

	changes := make([]FileChange, len(items))
	for i, item := range items {
		dec := json.NewDecoder(strings.NewReader(item))
		if err := dec.Decode(&changes[i]); err != nil {
			return nil, err
		}
	}

	return changes, nil
}

func (c Client) changeHandler() {
	fileChanges := []FileChange{}
	for {
		if len(fileChanges) == 0 {
			ch, err := c.dequeueFileChanges()
			if err != nil {
				log.Printf("Dequeue changes failed: %+v\n", err)
				return
			}

			fileChanges = append(fileChanges, ch...)
		}

		for _, ch := range fileChanges {
			log.Printf("TODO: handle file change %+v\n", ch)

			// TODO: Haven't written a file specific version yet
			//c.ForceSync(*site, db)
		}

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

	return c.enqueueFileChange(change)
}

func (c Client) changeWatcherRefresher(fileId string) {
	key := c.site.WebhookKey()

	ch, err := c.createChangeWatcher(fileId, key)
	if err != nil {
		log.Printf("ERROR: Failed to register file watcher: %+v\n", err)
	}

	if err := c.setResourceFile(fileId, ch.ResourceId); err != nil {
		log.Printf("ERROR: Failed to store resourceId -> fileId mapping %+v\n", err)
	}

	// Watchers expire every hour
	time.Sleep(60 * time.Minute)
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
