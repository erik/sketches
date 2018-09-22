// extensions to store which are specific to Google drive support.
package drive

import (
	"github.com/erik/gruppo/store"
)

const (
	keyFileFolder   = "drive:filetree:"
	keyResourceFile = "drive:resources:"
	keyFileSlug     = "drive:slugmap:"
)

// Return the directory a File is contained in. Because in google drive files
// don't know where they are.
func (c Client) getFileFolder(fileId string) (string, error) {
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

func (c Client) getSlugForFileId(fileId string) (string, error) {
	k := keyFileSlug + fileId
	return c.db.GetKey(k)
}

func (c Client) setSlugForFileId(fileId, slug string) error {
	k := keyFileSlug + fileId
	return c.db.SetKey(k, fileId)
}

func (c Client) pushFileChange(fc FileChange) error {
	k := store.KeyForSite(c.site, "drive:changes")
	return c.db.AddSetJSON(k, fc)
}

func (c Client) popFileChange() (*FileChange, error) {
	k := store.KeyForSite(c.site, "drive:changes")
	var change *FileChange

	if err := c.db.PopSetMember(k, change); err != nil {
		return nil, err
	}

	return change, nil
}
