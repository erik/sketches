// support for google drive's push notifications
package drive

import (
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"time"

	"github.com/google/uuid"
	log "github.com/sirupsen/logrus"
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
	for t := time.Tick(5 * time.Second); ; <-t {
		ch, err := c.popFileChange()
		if err != nil {
			log.WithError(err).Error("failed to dequeue changes")
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
			log.WithError(err).Error("failed to process file")
			return
		}

		log.WithField("post_data", post).
			Debug("handled file data")

		// Debounce
		time.Sleep(30 * time.Second)
	}
}

func (c Client) HandleChangeHook(req *http.Request) error {
	headers := req.Header

	resources := headers["X-Goog-Resource-Id"]
	if len(resources) < 1 {
		log.WithField("headers", headers).
			Warn("hook response from api missing resource id")

		return nil
	}

	resourceId := resources[0]

	state := headers["X-Goog-Resource-State"]
	if len(state) < 1 {
		log.WithField("headers", headers).
			Warn("hook response from api missing resource state")

		return nil
	}

	var changeKind string

	switch state[0] {
	case "sync":
		log.WithFields(log.Fields{
			"site":     c.site.HostPathPrefix(),
			"resource": resourceId,
		}).Info("registered hook successfully")

		return nil

	case "add":
		changeKind = fileChangeCreated

	case "update", "change":
		changeKind = fileChangeUpdated

	case "remove", "trash":
		changeKind = fileChangeDeleted

	default:
		log.WithFields(log.Fields{
			"site":     c.site.HostPathPrefix(),
			"state":    state,
			"resource": resourceId,
		}).Warn("unknown value for resource state")

		return nil
	}

	fileId, err := c.getResourceFile(resourceId)
	if err != nil {
		log.WithError(err).
			WithField("resource", resourceId).
			Error("failed to find fileId for resource")
		return err
	}

	path, err := c.getFileFolder(fileId)
	if err != nil {
		log.WithError(err).
			WithField("resource", resourceId).
			WithField("file", fileId).
			Error("failed to find folder for fileId")

		return err
	}

	log.WithFields(log.Fields{
		"site":        c.site.HostPathPrefix(),
		"file":        fileId,
		"folder":      path,
		"change_kind": changeKind,
		"resource":    resourceId,
	}).Info("adding file change")

	change := FileChange{
		Kind: changeKind,
		File: File{
			Id:   fileId,
			Path: path,
		},
	}

	return c.pushFileChange(change)
}

const webhookTimeout = 599 * time.Second

// Periodically recreate the webhook, since Drive's API only allows
// them to live briefly.
func (c Client) changeWatcherRefresher(fileId string) {
	key := c.site.WebhookKey()
	added, err := c.addWebhookIfNotExists(fileId, webhookTimeout)

	if !added || err != nil {
		log.WithField("file", fileId).
			Debug("not creating watcher, already exists")
		return
	}

	for t := time.Tick(webhookTimeout + 1); ; <-t {
		added, err := c.addWebhookIfNotExists(fileId, webhookTimeout)
		if !added || err != nil {
			log.WithField("file", fileId).
				Debug("not refreshing watcher, already exists")

			continue
		}

		ch, err := c.createChangeWatcher(fileId, key)

		if err != nil {
			log.WithError(err).
				WithField("site", c.site.HostPathPrefix()).
				Error("failed to register file watcher")

			continue
		}

		if err := c.setResourceFile(fileId, ch.ResourceId); err != nil {
			log.WithError(err).
				WithField("site", c.site.HostPathPrefix()).
				Error("failed to store resourceId -> fileId mapping")
		}
	}
}

func (c Client) createChangeWatcher(fileId string, key string) (*drive.Channel, error) {
	channel := &drive.Channel{
		Address: c.config.WatchNotificationURI + key,
		Id:      uuid.New().String(),
		Type:    "web_hook",
	}

	return c.service.Files.
		Watch(fileId, channel).
		Do()
}
