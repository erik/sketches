// extensions to store which are specific to Google drive support.
package drive

import (
	"encoding/json"
	"strings"
	"time"

	"github.com/go-redis/redis"
	log "github.com/sirupsen/logrus"

	"github.com/erik/gruppo/model"
	"github.com/erik/gruppo/store"
)

const (
	keyFileFolder   = "drive:filetree:"
	keyResourceFile = "drive:resources:"
	keyFileSlug     = "drive:slugmap:"
	keyWebhookFiles = "drive:webhooks:"
)

// Return the directory a File is contained in. Because in google drive files
// don't know where they are.
func (c Client) getFileFolder(fileId string) (string, error) {
	if fileId == c.site.Drive.FolderId {
		return "", nil
	}

	k := keyFileFolder + fileId
	return c.db.GetKey(k)
}

func (c Client) setFileFolder(fileId, path string) error {
	k := keyFileFolder + fileId
	return c.db.SetKey(k, path)
}

func (c Client) setResourceFile(fileId, resourceId string) error {
	k := keyResourceFile + resourceId
	return c.db.SetKey(k, fileId)
}

func (c Client) getResourceFile(resourceId string) (string, error) {
	k := keyResourceFile + resourceId
	return c.db.GetKey(k)
}

func (c Client) getOrSetSlugForFileId(fileId, slug string) (string, error) {
	k := keyFileSlug + fileId

	// Exclude key not found errors
	if val, err := c.db.GetKey(k); val != "" || (err != nil && err != redis.Nil) {
		return val, err
	}

	if err := c.db.SetKey(k, slug); err != nil {
		return "", err
	}

	return slug, nil
}

func (c Client) addWebhookIfNotExists(fileId string, t time.Duration) (bool, error) {
	k := keyWebhookFiles + fileId
	return c.db.SetKeyNX(k, "true", t)
}

func (c Client) pushDriveChange(dc DriveChange) error {
	k := store.KeyForSite(c.site, "drive:changes")
	return c.db.AddSetJSON(k, dc)
}

func (c Client) popDriveChange() (*DriveChange, error) {
	k := store.KeyForSite(c.site, "drive:changes")
	var change DriveChange

	if err := c.db.PopSetMember(k, &change); err != nil && err != redis.Nil {
		return nil, err
	} else if err == redis.Nil {
		return nil, nil
	}

	return &change, nil
}

func (c Client) addOrUpdatePost(p model.Post) error {
	k := store.KeyForSlug(c.site, p.Slug)

	var post model.Post

	// Merge post in db with new post. More complicated than it should
	// be, because go.
	// FIXME: also, it doesn't work.
	orig, err := c.db.GetPost(c.site, p.Slug)
	if err != nil && err != redis.Nil {
		return err
	} else if orig != nil {
		post = *orig
	}

	strPost, err := json.Marshal(p)
	if err != nil {
		return err
	}

	// Decode the new post "into" the existing post. This causes a merge.
	dec := json.NewDecoder(strings.NewReader(string(strPost)))
	if err := dec.Decode(&post); err != nil {
		return err
	}

	log.WithFields(log.Fields{
		"key":    k,
		"site":   c.site.HostPathPrefix(),
		"is_new": orig == nil,
	}).Debug("adding or updating post")

	return c.db.SetJSON(k, p)
}
