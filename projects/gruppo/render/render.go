package render

import (
	"path/filepath"

	"github.com/cbroglie/mustache"

	"github.com/erik/gruppo/model"
)

// TODO: This should be configured somehow
const (
	templateDir string = "./templates"
)

type Context struct {
	Title string
	Post  *model.Post
	Site  *model.Site
	Posts []model.PostOverview
}

func Render(page string, theme string, ctx *Context) (string, error) {
	mustache.AllowMissingVariables = false

	p := &mustache.FileProvider{
		Paths:      []string{filepath.Join(templateDir, theme)},
		Extensions: []string{"", ".html.tmpl", ".tmpl"},
	}

	f := filepath.Join(templateDir, theme, page+".html.tmpl")
	tmpl, err := mustache.ParseFilePartials(f, p)

	if err != nil {
		return "", err
	}

	return tmpl.Render(ctx)
}
