package providers

import (
	"io"
)

// TODO: Should be named something else
type Provider interface {
	List() (<-chan string, <-chan error)
	FetchFile(name string) (*io.Reader, error)
}
