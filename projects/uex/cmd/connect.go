package cmd

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"sync"
	"time"

	"gopkg.in/sorcix/irc.v2"
)

type client struct {
	conn *irc.Conn
	mux  sync.Mutex

	nick    string
	buffers map[string]chan *irc.Message

	directory string
}

func Connect() {
	hostname := "irc.freenode.net"
	port := 6667
	nick := "ep`uex"

	baseDir, err := ioutil.TempDir("", hostname)
	if err != nil {
		log.Fatal(err)
	}
	defer os.RemoveAll(baseDir)

	fmt.Printf("==> Output to %s\n", baseDir)

	for {
		client, err := createClient(hostname, port, baseDir)
		if err != nil {
			fmt.Printf("connect failed: %+v\n", err)
			goto retry
		}

		client.initialize(nick, "")

		// If we exit `runLoop` cleanly, it was an intentional process exit.
		if err := client.runLoop(); err == nil {
			break
		}

		fmt.Printf("IRC connection errored: %+v\n", err)
	retry:
		fmt.Println("... sleeping 5 seconds before reconnecting")
		time.Sleep(5 * time.Second)
	}
}

func createClient(hostname string, port int, baseDir string) (*client, error) {
	server := fmt.Sprintf("%s:%d", hostname, port)
	// TODO: handle TLS connection here as well
	conn, err := irc.Dial(server)
	if err != nil {
		return nil, err
	}

	client := &client{
		conn:      conn,
		directory: filepath.Join(baseDir, server),
	}

	return client, nil
}

func (c *client) initialize(nick, pass string) {
	if pass != "" {
		c.send("PASS", pass)
	}

	c.send("NICK", nick)
	c.send("USER", nick, "*", "*", "real name")

	c.nick = nick
}

func (c *client) send(cmd string, params ...string) {
	msg := &irc.Message{
		Command: cmd,
		Params:  params,
	}

	c.conn.Encode(msg)

	fmt.Printf("--> %+v\n", msg)
}

func (c *client) handleMessage(msg *irc.Message) {
	buf := c.getBuffer("server")

	switch msg.Command {
	case irc.PING:
		c.send(irc.PONG, msg.Params...)

	case irc.NICK:
		from := msg.Prefix.Name
		to := msg.Params[0]

		if from == c.nick {
			fmt.Printf("updating my nick to %s\n", to)
			c.nick = to
		} else {
			// TODO: broadcast renames to all bufs having that user?
			// requires more tracking
			buf = nil
		}

	case irc.JOIN:
		buf = c.getBuffer(msg.Params[0])

	case irc.PRIVMSG, irc.NOTICE:
		// TODO: direct messages
		buf = c.getBuffer(msg.Params[0])
	}

	if buf != nil {
		buf <- msg
	}
}

func (c *client) getBuffer(name string) chan *irc.Message {
	c.mux.Lock()
	defer c.mux.Unlock()

	if name == "*" {
		name = "server"
	}

	if ch, exists := c.buffers[name]; exists {
		return ch
	}

	path := filepath.Join(c.directory, name)
	if err := os.MkdirAll(path, os.ModePerm); err != nil {
		log.Fatal(err)
	}

	ch := make(chan *irc.Message)
	go c.bufferInputHandler(path)
	go c.bufferOutputHandler(path, ch)

	return ch
}

func (c *client) bufferOutputHandler(path string, ch chan *irc.Message) {
	n := filepath.Join(path, "__out")
	m := os.O_APPEND | os.O_RDWR | os.O_CREATE
	f, err := os.OpenFile(n, m, 0644)
	if err != nil {
		log.Fatal(err)
	}

	defer f.Close()

	// TODO: better serialization?? colors?? etc.
	for msg := range ch {
		if _, err := f.WriteString(fmt.Sprintf(">> %+v\n", msg)); err != nil {
			log.Fatal(err)
		}
		if err := f.Sync(); err != nil {
			log.Fatal(err)
		}
	}
}

func (c *client) bufferInputHandler(path string) {

}

func (c *client) runLoop() error {
	for {
		message, err := c.conn.Decode()
		if err != nil {
			return err
		}

		fmt.Printf("<-- %+v\n", message)
		c.handleMessage(message)
	}
}
