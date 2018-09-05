package providers

import (
	"io"
)

// TODO: Should be named something else
type Provider interface {
	List(directory string) (<-chan string, <-chan error)
	ExportFile(name string) (*io.Reader, error)
}
