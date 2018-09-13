package web

import (
	"fmt"
	"net/http"

	"github.com/labstack/echo"
	"github.com/labstack/echo/middleware"
)

type Configuration struct {
	Host         string
	Port         int
	TemplatePath string
}

func registerRoutes(e *echo.Echo) {
	// General purpose
	// ...
	e.GET("/", func(c echo.Context) error {
		return c.String(http.StatusOK, "Hello, World!")
	})

	// Site-specific
	// ...
}

func registerMiddleware(e *echo.Echo) {
	e.Use(middleware.Logger())
	e.Use(middleware.Recover())
}

type web struct {
	echo *echo.Echo
	conf *Configuration
}

func New(conf Configuration) web {
	e := echo.New()

	registerMiddleware(e)
	registerRoutes(e)

	return web{e, &conf}
}

func (w web) Serve() error {
	address := fmt.Sprintf("%s:%d", w.conf.Host, w.conf.Port)

	return w.echo.Start(address)
}
