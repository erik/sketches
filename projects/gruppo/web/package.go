package web

import (
	"fmt"
	"log"
	"net/http"
	"path/filepath"
	"sort"
	"strings"

	"github.com/labstack/echo"
	"github.com/labstack/echo/middleware"

	"github.com/erik/gruppo/model"
	"github.com/erik/gruppo/render"
	"github.com/erik/gruppo/store"
)

type Configuration struct {
	Host         string
	Port         int
	TemplatePath string
}

type siteMapping map[string][]model.Site

type bySitePathLen []model.Site

func (s bySitePathLen) Len() int {
	return len(s)
}
func (s bySitePathLen) Swap(i, j int) {
	s[i], s[j] = s[j], s[i]
}
func (s bySitePathLen) Less(i, j int) bool {
	return len(s[i].BasePath) > len(s[j].BasePath)
}

func buildSiteMap(sites []model.Site) siteMapping {
	var m siteMapping = make(siteMapping, len(sites))

	for _, s := range sites {
		if _, ok := m[s.Host]; !ok {
			m[s.Host] = []model.Site{}
		}

		m[s.Host] = append(m[s.Host], s)
	}

	for _, v := range m {
		sort.Sort(bySitePathLen(v))
	}

	log.Printf("site map => %+v", m)
	return m
}

type web struct {
	echo  *echo.Echo
	db    *store.RedisStore
	conf  *Configuration
	sites siteMapping // host -> [site, ...]
}

func (w *web) registerRoutes(e *echo.Echo) {
	// General purpose
	// ...
	e.GET("/panel", func(c echo.Context) error {
		return c.String(http.StatusOK, "Hello, World!")
	})

	// Site-specific
	e.GET("/*", func(c echo.Context) error {
		site := w.siteFromContext(c)

		if site == nil {
			return c.String(http.StatusNotFound, "unknown site")
		}

		return w.handleSiteRequest(site, c)
	})
}

func pageForSlug(site *model.Site, slug string) *model.PageConfig {
	for _, pg := range site.Pages {
		if pg.URL == slug {
			return &pg
		}
	}
	return nil
}

func assetForSlug(site *model.Site, slug string) string {
	if strings.HasPrefix(slug, site.AssetPath) {
		return strings.TrimPrefix(slug, site.AssetPath)
	}

	return ""
}

func (w *web) handlePage(site *model.Site, pg *model.PageConfig, c echo.Context) error {
	html, err := render.Render(pg.Template, site.Theme, &render.Context{
		Title: pg.Title,
	})

	if err != nil {
		log.Printf("Rendering failed: %+v\n", err)
		return err
	}

	return c.HTML(http.StatusOK, html)
}

func (w *web) handlePost(site *model.Site, slug string, c echo.Context) error {
	post, err := w.db.GetPost(*site, slug)
	if post == nil {
		return c.String(http.StatusNotFound, "404.")
	}

	if err != nil {
		return err
	}

	html, err := render.Render("post", site.Theme, &render.Context{
		Title: post.Title,
		Post:  post,
	})

	return c.HTML(http.StatusOK, html)
}

func (w *web) handleAsset(site *model.Site, slug string, c echo.Context) error {
	// FIXME: this might be vulnerable to directory traversal
	slug = filepath.Clean(slug)

	path := filepath.Join(site.SiteDir, "assets", slug)

	return c.File(path)
}

func (w *web) handleSiteRequest(site *model.Site, c echo.Context) error {
	slug := strings.TrimPrefix(c.Request().URL.String(), site.BasePath)

	// Slugs should be relative
	if slug != "/" && strings.HasPrefix(slug, "/") {
		slug = slug[1:]
	} else if slug == "" {
		slug = "/"
	}

	if pg := pageForSlug(site, slug); pg != nil {
		return w.handlePage(site, pg, c)
	}

	if asset := assetForSlug(site, slug); asset != "" {
		return w.handleAsset(site, asset, c)
	}

	return w.handlePost(site, slug, c)
}

func (w *web) registerMiddleware(e *echo.Echo) {
	e.Use(middleware.Logger())
	e.Use(middleware.Recover())
}

func (w *web) siteFromContext(c echo.Context) *model.Site {
	for host, sites := range w.sites {
		if host != c.Request().Host {
			continue
		}

		for _, site := range sites {
			path := c.Request().URL.String()
			if strings.HasPrefix(path, site.BasePath) {
				return &site
			}
		}
	}

	return nil
}

func New(sites []model.Site, conf Configuration, db *store.RedisStore) web {
	e := echo.New()
	s := buildSiteMap(sites)

	w := web{e, db, &conf, s}

	w.registerMiddleware(e)
	w.registerRoutes(e)

	return w
}

func (w web) Serve() error {
	address := fmt.Sprintf("%s:%d", w.conf.Host, w.conf.Port)

	return w.echo.Start(address)
}
