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

func (c Client) handleFileChange(file File) error {
	dir, err := ioutil.TempDir("/tmp", "exported-media-")
	if err != nil {
		log.Fatal(err)
	}

	defer os.RemoveAll(dir)

	if _, err := c.processFile(file, dir); err != nil {
		log.WithError(err).Error("failed to process file")
		return err
	}

	return nil
}

func (c Client) handleFolderChange(folderId, path string) error {
	return c.syncFolder(folderId, path, false)
}

// Continuously listen for file changes in Redis, processing files as
// they appear. Applies a debounce so that multiple quick edits to the
// same file are not consuming the worker.
func (c Client) changeHandler() {
	timer := time.NewTimer(5 * time.Second)
	defer timer.Stop()

	for range timer.C {
		ch, err := c.popDriveChange()
		if err != nil {
			log.WithError(err).Error("failed to dequeue changes")
			return
		}

		// If we don't have any work, sleep a little less
		if ch == nil {
			continue
		}

		file, err := c.getFileMeta(ch.FileId)
		if err != nil {
			log.WithError(err).
				WithField("fileId", ch.FileId).
				Error("failed to look up file metadata")

			continue
		}

		switch file.MimeType {
		case MimeTypeDriveFolder:
			_ = c.handleFolderChange(file.Id, ch.Path)

		default:
			f := fileFromAPI(file, ch.Path)
			_ = c.handleFileChange(f)
		}

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
	if state == nil || len(state) < 1 {
		log.WithField("headers", headers).
			Warn("hook response from api missing resource state")

		return nil
	}

	// Sync messages indicate that the webhook creation was successful,
	// nothing to do.
	if state[0] == "sync" {
		return nil
	}

	// TODO: probably need to handle deletions here

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
		"change_kind": state[0],
		"resource":    resourceId,
	}).Info("adding file change")

	change := DriveChange{
		FileId: fileId,
		Path:   path,
	}

	return c.pushDriveChange(change)
}

const webhookTimeout = 599 * time.Second

// Periodically recreate the webhook, since Drive's API only allows
// them to live briefly.
func (c Client) changeWatcherRefresher(fileId string) {
	key := c.site.WebhookKey()

	// FIXME: Theoretically, we could miss some updates during the 1 second.
	t := time.NewTicker(webhookTimeout + 1*time.Second)
	defer t.Stop()

	for range t.C {
		set, err := c.addWebhookIfNotExists(fileId, webhookTimeout)
		if !set || err != nil {
			log.WithField("file", fileId).
				WithError(err).
				Debug("not refreshing watcher, already exists")

			return
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
