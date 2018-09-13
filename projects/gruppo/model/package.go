// Various structs that are serialized into the database.
package model

type Post struct {
	Id       string
	Title    string
	Subtitle string
	Author   string
	Date     string

	Content    string
	ImagePaths []string
}

type User struct {
}

type SiteDriveConfig struct {
	FolderId string `json:"folder_id"`
	Token    string `json:"token"` // serialized oauth token
}

type PageConfig struct {
	URL    string `json:"url"`
	Title  string `json:"title"`
	PostId string `json:"post_id"`
}

type Site struct {
	Host     string       `json:"host"`
	BasePath string       `json:"base_url"`
	Pages    []PageConfig `json:"pages"`
	SiteDir  string       `json:"content_dir"`

	Drive *SiteDriveConfig `json:"drive"`
}
