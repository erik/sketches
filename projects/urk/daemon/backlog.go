package daemon

import (
	"encoding/json"
	"io"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strings"
	"time"

	"gopkg.in/sorcix/irc.v1"
)

type Backlog struct {
	directory string
	targets   map[string]io.ReadWriter
	metadata  *Metadata
}

type Metadata struct {
	Channels    map[string]*ChannelMetadata
	CurrentNick string
}

type ChannelMetadata struct {
	Active bool             // Are we currently in this channel?
	Nicks  map[string]int64 // map of nick -> last seen ts
	Topic  string
}

const serverLogFile = "server.log"

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

		if err := ioutil.WriteFile(filename, marshal, 0700); err != nil {
			log.Fatal(err)
		}
	}
}

func NewBacklog(logdir string, address string) *Backlog {
	directory := filepath.Join(logdir, address)

	if err := os.MkdirAll(directory, 0700); err != nil {
		log.Fatal("Couldn't create backlog directory:", err)
	}

	meta := &Metadata{
		Channels:    make(map[string]*ChannelMetadata),
		CurrentNick: "",
	}

	go meta.sync(directory)

	return &Backlog{
		directory: directory,
		targets:   make(map[string]io.ReadWriter, 10),
		metadata:  meta,
	}
}

func (b *Backlog) getTarget(target string) io.ReadWriter {
	if b.targets[target] == nil {
		filename := filepath.Join(b.directory, target)

		rw, err := os.OpenFile(filename, os.O_CREATE|os.O_APPEND|os.O_RDWR, 0700)
		if err != nil {
			log.Fatal("Couldn't do it", err)
		}

		b.targets[target] = rw
	}

	return b.targets[target]
}

func (b *Backlog) getMetadata(target string) *ChannelMetadata {
	log.Printf("Getting metadata for %s", target)
	if b.metadata.Channels[target] == nil {
		b.metadata.Channels[target] = &ChannelMetadata{
			Nicks: make(map[string]int64, 1),
			Topic: "",
		}
	}

	return b.metadata.Channels[target]
}

func (b *Backlog) AddMessage(msg *irc.Message) error {
	log.Printf("ok adding msg=%s, params=%v", msg.String(), msg.Params)

	switch msg.Command {
	case irc.PRIVMSG, irc.NOTICE:
		target := msg.Params[0]

		// Don't generate a file named "*"
		if target == "*" {
			target = serverLogFile
		}

		rw := b.getTarget(target)
		rw.Write([]byte(msg.String() + "\n"))

		meta := b.getMetadata(msg.Params[0])
		meta.Nicks[msg.Name] = time.Now().Unix()
		break

	case irc.RPL_NAMREPLY:
		meta := b.getMetadata(msg.Params[0])

		for _, n := range strings.Split(msg.Trailing, " ") {
			// remove Oper etc markings
			// TODO: make this an exhaustive set and factor out
			bare_nick := strings.TrimLeft(n, "~@+")

			meta.Nicks[bare_nick] = time.Now().Unix()
		}

		break

	case irc.RPL_TOPIC:
		data := b.getMetadata(msg.Params[0])
		data.Topic = msg.Trailing
		break

	case irc.JOIN, irc.PART:
		// FIXME: This is stupid brittle, just us JOIN/PARTing
		if len(msg.Params) == 1 {
			meta := b.getMetadata(msg.Params[0])
			meta.Active = (msg.Command == irc.JOIN)
		}

		meta := b.getMetadata(msg.Params[1])
		meta.Nicks[msg.Name] = time.Now().Unix()
		break

	default:
		rw := b.getTarget(serverLogFile)
		rw.Write([]byte(msg.String() + "\n"))
	}

	return nil
}
