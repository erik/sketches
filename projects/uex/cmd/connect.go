package cmd

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"time"

	"gopkg.in/sorcix/irc.v2"
)

type client struct {
	conn *irc.Conn
	mux  sync.Mutex

	nick    string
	buffers map[string]buffer

	directory string
}

type buffer struct {
	ch     chan *irc.Message
	client *client
	path   string

	name  string
	topic string
	users map[string]string
}

const serverBufferName = "$server"

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
		buffers:   make(map[string]buffer),
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
	buf := c.getBuffer(serverBufferName)

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
		target := msg.Params[0]

		// Group all messages sent by the server together,
		// regardless of server name.
		//
		// For direct messages, we want to look at the sender.
		if msg.Prefix.IsServer() {
			target = serverBufferName
		} else if !isChannel(target) {
			target = msg.Prefix.Name
		}

		buf = c.getBuffer(target)

	case irc.RPL_TOPIC:
		target := msg.Params[1]
		topic := msg.Params[2]

		buf = c.getBuffer(target)
		buf.topic = topic
	}

	if buf != nil {
		buf.ch <- msg
	}
}

func (c *client) getBuffer(name string) *buffer {
	c.mux.Lock()
	defer c.mux.Unlock()

	// Sent early on, at least by freenode.
	if name == "*" {
		name = serverBufferName
	}

	if buf, exists := c.buffers[name]; exists {
		return &buf
	}

	path := c.directory

	// We want to write __in, __out top level for the server, and
	// as a child for every other buffer.
	if name != serverBufferName {
		path = filepath.Join(path, name)
	}

	if err := os.MkdirAll(path, os.ModePerm); err != nil {
		log.Fatal(err)
	}

	c.buffers[name] = buffer{
		ch:     make(chan *irc.Message),
		client: c,
		path:   path,

		name:  name,
		topic: "",
		users: make(map[string]string),
	}

	b := c.buffers[name]

	go b.inputHandler()
	go b.outputHandler()

	return &b
}

func (b *buffer) outputHandler() {
	name := filepath.Join(b.path, "__out")
	mode := os.O_APPEND | os.O_RDWR | os.O_CREATE
	file, err := os.OpenFile(name, mode, 0644)
	if err != nil {
		log.Fatal(err)
	}

	defer file.Close()

	// TODO: better serialization?? colors?? etc.
	for msg := range b.ch {
		if _, err := file.WriteString(fmt.Sprintf(">> %+v\n", msg)); err != nil {
			log.Fatal(err)
		}
		if err := file.Sync(); err != nil {
			log.Fatal(err)
		}
	}
}

func (b *buffer) inputHandler() {
	name := filepath.Join(b.path, "__in")
	err := syscall.Mkfifo(name, 0777)

	// Doesn't matter if the FIFO already exists from a previous run.
	if err != nil && err != syscall.EEXIST {
		log.Fatal(err)
	}

	for {
		buf, err := ioutil.ReadFile(name)
		if err != nil {
			log.Fatal(err)
		}

		if len(buf) == 0 {
			time.Sleep(100 * time.Millisecond)
			continue
		}

		lines := strings.Split(string(buf), "\n")
		for _, line := range lines {
			line = strings.TrimSpace(line)
			if line == "" {
				continue
			}

			b.client.handleInputLine(b.name, line)
		}
	}
}

func isChannel(target string) bool {
	if target == "" {
		return false
	}

	return target[0] == '#' || target[0] == '&'
}

func splitInputCommand(bufName, line string) (string, string) {
	// Without a prefix, it's just a regular PRIVMSG
	if !strings.HasPrefix(line, "/") {
		return "/msg", bufName + " " + line
	}
	// Double slash at start means privmsg with leading slash
	if strings.HasPrefix(line, "//") {
		return "/msg", bufName + " " + line[1:]
	}

	s := strings.SplitN(line, " ", 2)
	cmd := strings.ToLower(s[0])

	if len(s) == 1 {
		return cmd, ""
	}

	return cmd, s[1]
}

func (c *client) handleInputLine(bufName, line string) {
	fmt.Printf("%s >> %s\n", bufName, line)

	cmd, rest := splitInputCommand(bufName, line)

	switch cmd {
	case "/m", "/msg":
		s := strings.SplitN(rest, " ", 2)
		if len(s) != 2 {
			fmt.Printf("expected: /msg TARGET MESSAGE")
			return
		}
		c.send("PRIVMSG", s...)

	case "/j", "/join":
		if !isChannel(rest) {
			fmt.Printf("expected: /join TARGET")
			return
		}
		c.send("JOIN", rest)

	case "/quote":
		params := strings.Split(rest, " ")
		if len(params) == 1 {
			c.send(params[1])
		} else {
			c.send(params[1], params[1:]...)
		}

	default:
		fmt.Printf("Unknown command: %s %s\n", cmd, rest)
	}
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
