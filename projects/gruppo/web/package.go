package web

import (
	"errors"
	"fmt"
	"log"
	"net/http"
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
		site, err := w.siteFromContext(c)

		if err != nil {
			return err
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
	if err != nil || post == nil {
		log.Printf("failed with error: %+v\n", err)
		return c.String(http.StatusNotFound, "404.")
	}

	html, err := render.Render("post", site.Theme, &render.Context{
		Title: post.Title,
		Post:  post,
	})

	return c.HTML(http.StatusOK, html)
}

func (w *web) handleSiteRequest(site *model.Site, c echo.Context) error {
	slug := strings.TrimPrefix(c.Request().URL.String(), site.BasePath)

	if pg := pageForSlug(site, slug); pg != nil {
		return w.handlePage(site, pg, c)
	}

	return w.handlePost(site, slug, c)
}

func (w *web) registerMiddleware(e *echo.Echo) {
	e.Use(middleware.Logger())
	e.Use(middleware.Recover())
}

func (w *web) siteFromContext(c echo.Context) (*model.Site, error) {
	for host, sites := range w.sites {
		if host != c.Request().Host {
			continue
		}

		for _, site := range sites {
			path := c.Request().URL.String()
			if strings.HasPrefix(path, site.BasePath) {
				return &site, nil
			}
		}
	}

	return nil, errors.New("unknown site mapping")
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
