package web

import (
	"fmt"
	"net/http"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/labstack/echo"
	"github.com/labstack/echo/middleware"
	log "github.com/sirupsen/logrus"

	"github.com/erik/gruppo/drive"
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

type Web struct {
	echo  *echo.Echo
	db    *store.RedisStore
	conf  Configuration
	sites siteMapping // host -> [site, ...]
}

// buildSiteMap generates a mapping of `host -> [site, ...]`, with sites sorted
// by the longest base path. This is to ensure e.g. example.com/foobar/ matches
// before example.com/foo
func buildSiteMap(sites []model.Site) siteMapping {
	var m siteMapping = make(siteMapping, len(sites))

	for _, s := range sites {
		if _, ok := m[s.Host]; !ok {
			m[s.Host] = []model.Site{}
		}

		m[s.Host] = append(m[s.Host], s)
	}

	for _, v := range m {
		// We want to match the longest path first.
		sort.Slice(v, func(i, j int) bool {
			return len(v[i].BasePath) > len(v[j].BasePath)
		})
	}

	log.WithFields(log.Fields{"map": m}).Debug("built site map")

	return m
}

func logger() echo.MiddlewareFunc {
	return func(next echo.HandlerFunc) echo.HandlerFunc {
		return func(c echo.Context) error {
			req := c.Request()
			res := c.Response()
			start := time.Now()

			var err error
			if err = next(c); err != nil {
				c.Error(err)
			}

			stop := time.Now()

			log.Infof("%s %s %s %s - %d [%s]",
				req.Host,
				c.RealIP(),
				req.Method,
				req.RequestURI,
				res.Status,
				stop.Sub(start).String(),
			)

			return err
		}
	}
}

func (w *Web) RegisterDriveHooks(c *drive.Client) error {
	route := c.ChangeHookRoute()

	log.WithFields(log.Fields{}).Info("setting up drive hook")

	w.echo.POST(route, func(ctx echo.Context) error {
		if err := c.HandleChangeHook(ctx.Request()); err != nil {
			log.WithError(err).
				WithFields(log.Fields{}).
				Error("failed to handle change hook")

			return ctx.String(http.StatusInternalServerError, "something bad")
		}

		return ctx.String(http.StatusOK, "")
	})

	return nil
}

func (w *Web) registerRoutes() {
	// Site-specific
	w.echo.GET("/*", func(c echo.Context) error {
		site, found := w.siteForContext(c)

		if !found {
			return c.String(http.StatusNotFound, "unknown site")
		}

		return w.handleSiteRequest(site, c)
	})
}

// Look up the correct Site configuration for a given request by matching host
// and path.
func (w Web) siteForContext(c echo.Context) (model.Site, bool) {
	for host, sites := range w.sites {
		if host != c.Request().Host {
			continue
		}

		for _, site := range sites {
			path := c.Request().URL.String()
			if strings.HasPrefix(path, site.BasePath) {
				return site, true
			}
		}
	}

	return model.Site{}, false
}

func pageForSlug(site model.Site, slug string) (model.PageConfig, bool) {
	for _, pg := range site.Pages {
		if pg.URL == slug {
			return pg, true
		}
	}

	return model.PageConfig{}, false
}

func assetForSlug(site model.Site, slug string) (string, bool) {
	if strings.HasPrefix(slug, site.AssetPath) {
		return strings.TrimPrefix(slug, site.AssetPath), true
	}

	return "", false
}

const pageSize = 10

func (w Web) handlePage(site model.Site, pg model.PageConfig, slug string, c echo.Context) error {
	var page int
	if _, err := fmt.Sscanf(c.QueryParam("page"), "%d", &page); err != nil {
		page = 0
	}

	offset := page * pageSize
	posts, err := w.db.ListPostOverviews(site, slug, offset, pageSize)
	if err != nil {
		return err
	}

	html, err := render.Render(pg.Template, site.Theme, &render.Context{
		Title: pg.Title,
		Site:  &site,
		Posts: posts,
	})

	if err != nil {
		log.WithFields(log.Fields{
			"error":    err,
			"site":     site.HostPathPrefix(),
			"template": pg.Template,
			"theme":    site.Theme,
		}).Error("failed to render page template")

		return err
	}

	return c.HTML(http.StatusOK, html)
}

func (w Web) handlePost(site model.Site, post model.Post, c echo.Context) error {
	html, err := render.Render("post", site.Theme, &render.Context{
		Title: post.Title,
		Post:  &post,
	})

	if err != nil {
		log.WithFields(log.Fields{
			"error": err,
			"site":  site.HostPathPrefix(),
			"theme": site.Theme,
		}).Error("failed to render post template")

		return c.String(http.StatusInternalServerError, "failed to render")
	}

	return c.HTML(http.StatusOK, html)
}

func (w Web) handleAsset(site model.Site, slug string, c echo.Context) error {
	// FIXME: this might be vulnerable to directory traversal
	slug = filepath.Clean(slug)
	path := filepath.Join(site.SiteDir, "assets", slug)
	return c.File(path)
}

// Main URL routing dispatch.
func (w Web) handleSiteRequest(site model.Site, c echo.Context) error {
	slug := strings.TrimPrefix(c.Request().URL.Path, site.BasePath)

	// Slugs should be absolute
	if !strings.HasPrefix(slug, "/") {
		slug = "/" + slug
	}

	if pg, found := pageForSlug(site, slug); found {
		return w.handlePage(site, pg, slug, c)
	}

	if asset, found := assetForSlug(site, slug); found {
		return w.handleAsset(site, asset, c)
	}

	if post, err := w.db.GetPost(site, slug); post != nil {
		return w.handlePost(site, *post, c)
	} else if err != store.NotFound {
		log.WithFields(log.Fields{
			"error": err,
			"site":  site.HostPathPrefix(),
			"slug":  slug,
		}).Error("failed to fetch post")

		return c.String(http.StatusInternalServerError, "something went wrong")
	}

	if slugs, err := w.db.ListMatchingSlugs(site, slug); len(slugs) > 0 {
		if site.IndexPage != nil && len(slugs) > 0 {
			pg := model.PageConfig{
				URL:      slug,
				Template: site.IndexPage.Template,
				Title:    site.IndexPage.Title,
			}

			return w.handlePage(site, pg, slug, c)
		}

	} else if err != nil {
		log.WithFields(log.Fields{
			"error": err,
			"site":  site.HostPathPrefix(),
			"slug":  slug,
		}).Error("something went wrong")

		return c.String(http.StatusInternalServerError, "something went wrong")
	}

	return c.String(http.StatusNotFound, "404.")
}

func (w *Web) registerMiddleware() {
	// Echo's logger sucks, use a custom one
	w.echo.Use(logger())
	w.echo.Use(middleware.Recover())
}

func New(sites []model.Site, conf Configuration, db *store.RedisStore) Web {
	e := echo.New()
	e.HideBanner = true

	w := Web{
		echo:  e,
		db:    db,
		conf:  conf,
		sites: buildSiteMap(sites),
	}

	w.registerMiddleware()
	w.registerRoutes()

	return w
}

func (w Web) Serve() error {
	address := fmt.Sprintf("%s:%d", w.conf.Host, w.conf.Port)

	return w.echo.Start(address)
}
