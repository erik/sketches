package providers

import (
	"io"
)

type ProviderFile struct {
	Id     string
	Name   string
	Path   string
	Author string
}

// TODO: Should be named something else
type Provider interface {
	List(directory string) (<-chan ProviderFile, <-chan error)
	ExportAsDocx(file ProviderFile) (io.Reader, error)
}
