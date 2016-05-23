package daemon

import (
	"encoding/json"
	"io"
	"log"
	"os"
	"path/filepath"
	"time"

	"gopkg.in/sorcix/irc.v1"
)

type Backlog struct {
	directory string
	targets   map[string]io.ReadWriter
	metadata  *Metadata
}

type Metadata struct {
	channels    map[string]ChannelMetadata
	currentNick string
}

type ChannelMetadata struct {
	nicks map[string]bool
	topic string
}

// Sync on disk and in memory representation of metadata
func (m *Metadata) sync(directory string) {
	filename := filepath.Join(directory, "metadata.json")
	ticker := time.NewTicker(15 * time.Second)

	for {
		<-ticker.C

		marshal, err := json.Marshal(m)
		if err != nil {
			log.Println("Couldn't serialize json", err)
		}

		fp, err := os.Open(filename)
		if err != nil {
			log.Fatal(err)
		}

		fp.Write(marshal)
	}
}

func NewBacklog(logdir string, address string) *Backlog {
	directory := filepath.Join(logdir, address)

	if err := os.MkdirAll(directory, 0700); err != nil {
		log.Fatal("Couldn't create backlog directory:", err)
	}

	meta := &Metadata{
		channels:    make(map[string]ChannelMetadata),
		currentNick: "",
	}

	go meta.sync(directory)

	return &Backlog{
		directory: directory,
		targets:   make(map[string]io.ReadWriter, 10),
		metadata:  meta,
	}
}

func (b *Backlog) AddMessage(msg *irc.Message) error {

	switch msg.Command {
	case irc.PRIVMSG, irc.NOTICE:
		break
	case irc.JOIN:
		break
	case irc.PART:
		break

	}
	return nil
}
