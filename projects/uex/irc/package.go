package irc

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"

	"gopkg.in/sorcix/irc.v2"
)

type Config struct {
	Host  string
	Port  int
	IsTLS bool

	Directory string

	Nick       string
	RealName   string
	ServerPass string
	OnConnect  []string
}

type Client struct {
	Config

	conn *irc.Conn
	mux  sync.Mutex

	buffers map[string]buffer

	directory string
}

type buffer struct {
	ch     chan *irc.Message
	client *Client
	path   string

	name  string
	topic string
	users map[string]string
}

const serverBufferName = "$server"

func NewClient(cfg Config) (*Client, error) {
	var conn *irc.Conn
	var err error

	server := fmt.Sprintf("%s:%d", cfg.Host, cfg.Port)
	if cfg.IsTLS {
		conn, err = irc.DialTLS(server, nil)
	} else {
		conn, err = irc.Dial(server)
	}

	if err != nil {
		return nil, err
	}

	client := &Client{
		Config: cfg,

		conn:      conn,
		directory: filepath.Join(cfg.Directory, server),
		buffers:   make(map[string]buffer),
	}

	return client, nil
}

func (c *Client) Initialize() {
	if c.ServerPass != "" {
		c.send("PASS", c.ServerPass)
	}

	r := c.RealName
	if r == "" {
		r = c.Nick
	}

	c.send("NICK", c.Nick)
	c.send("USER", c.Nick, "*", "*", c.RealName)
}

func (c *Client) send(cmd string, params ...string) {
	msg := &irc.Message{
		Command: cmd,
		Params:  params,
	}

	c.conn.Encode(msg)

	fmt.Printf("--> %+v\n", msg)
}

func (c *Client) sendRaw(msg string) {
	c.conn.Write([]byte(msg))
	fmt.Printf("--> %+v\n", msg)
}

func (c *Client) handleMessage(msg *irc.Message) {
	buf := c.getBuffer(serverBufferName)

	switch msg.Command {
	case irc.RPL_WELCOME:
		for _, msg := range c.OnConnect {
			c.sendRaw(msg)
		}

	case irc.PING:
		c.send(irc.PONG, msg.Params...)

	case irc.PONG:
		if len(msg.Params) != 2 {
			break
		}

		s := strings.SplitN(msg.Params[1], " ", 2)
		if len(s) != 2 {
			break
		}

		// TODO: Write this to buffer.
		if ts, err := strconv.ParseInt(s[1], 10, 64); err == nil {
			delta := time.Duration(time.Now().UnixNano()-ts) / time.Millisecond
			fmt.Printf("PONG from %s: %d ms\n", msg.Params[0], delta)
		}

	case irc.NICK:
		from := msg.Prefix.Name
		to := msg.Params[0]

		if from == c.Nick {
			fmt.Printf("updating my nick to %s\n", to)
			c.Nick = to
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

	case irc.ERR_NICKNAMEINUSE:
		c.Nick = c.Nick + "`"
		fmt.Printf("Nick in use, trying '%s'\n", c.Nick)
		c.send("NICK", c.Nick)
	}

	if buf != nil {
		buf.ch <- msg
	}
}

func (c *Client) getBuffer(name string) *buffer {
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
		log.Fatalf("failed to create output file: %+v\n", err)
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

func (c *Client) handleInputLine(bufName, line string) {
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

	case "/ping":
		ts := time.Now().UnixNano()
		c.send("PING", fmt.Sprintf("%s %d", bufName, ts))

	case "/quote":
		params := strings.Split(rest, " ")
		if len(params) == 1 {
			c.send(params[0])
		} else {
			c.send(params[0], params[1:]...)
		}

	default:
		fmt.Printf("Unknown command: %s %s\n", cmd, rest)
	}
}

func (c *Client) RunLoop() error {
	for {
		message, err := c.conn.Decode()
		if err != nil {
			return err
		}

		fmt.Printf("<-- %+v\n", message)
		c.handleMessage(message)
	}
}
