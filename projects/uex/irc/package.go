package irc

import (
	"crypto/tls"
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

type ClientConfiguration struct {
	Name                 string `json:"name"`
	Host                 string `json:"host"`
	Port                 int    `json:"port"`
	IsTLS                bool   `json:"is_tls"`
	SkipCertificateCheck bool   `json:"skip_certificate_check,omitempty"`

	Nick           string   `json:"nick"`
	RealName       string   `json:"real_name"`
	ServerPass     string   `json:"server_pass,omitempty"`
	OnConnect      []string `json:"on_connect"`
	RejoinExisting bool     `json:"rejoin_existing"`
}

type Client struct {
	ClientConfiguration

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

func NewClient(baseDir string, cfg ClientConfiguration) *Client {
	client := &Client{
		ClientConfiguration: cfg,

		directory: filepath.Join(baseDir, cfg.Name),
		buffers:   make(map[string]buffer),
	}

	return client
}

func (c *Client) Initialize() error {
	server := fmt.Sprintf("%s:%d", c.Host, c.Port)

	var err error
	if c.IsTLS {
		t := &tls.Config{InsecureSkipVerify: c.SkipCertificateCheck}
		c.conn, err = irc.DialTLS(server, t)
	} else {
		c.conn, err = irc.Dial(server)
	}

	if err != nil {
		return err
	}

	if c.ServerPass != "" {
		c.send("PASS", c.ServerPass)
	}

	r := c.RealName
	if r == "" {
		r = c.Nick
	}

	c.send("NICK", c.Nick)
	c.send("USER", c.Nick, "*", "*", c.RealName)

	return nil
}

func (c *Client) send(cmd string, params ...string) {
	msg := &irc.Message{
		Command: cmd,
		Params:  params,
	}

	c.conn.Encode(msg)

	fmt.Printf("[%s] --> %+v\n", c.Name, msg)
}

func (c *Client) sendRaw(msg string) {
	c.conn.Write([]byte(msg))
	fmt.Printf("[%s] --> %+v\n", c.Name, msg)
}

func (c *Client) listExistingChannels() []string {
	files, err := ioutil.ReadDir(c.directory)
	if err != nil {
		log.Fatal(err)
	}

	channels := []string{}
	for _, file := range files {
		name := file.Name()
		if isChannel(name) {
			channels = append(channels, name)
		}
	}

	return channels
}

func (b *buffer) writeInfoMessage(msg string) {
	b.ch <- &irc.Message{
		Prefix:  &irc.Prefix{Name: "uex"},
		Command: "*",
		Params:  []string{msg},
	}

}

func (c *Client) handleMessage(msg *irc.Message) {
	buf := c.getBuffer(serverBufferName)

	switch msg.Command {
	case irc.RPL_WELCOME:
		for _, msg := range c.OnConnect {
			c.sendRaw(msg)
		}

		if c.RejoinExisting {
			for _, ch := range c.listExistingChannels() {
				c.send(irc.JOIN, ch)
			}
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
			text := fmt.Sprintf("PONG from %s: %d ms\n", msg.Params[0], delta)

			buf.writeInfoMessage(text)
		}

	case irc.NICK:
		from := msg.Prefix.Name
		to := msg.Params[0]

		if from == c.Nick {
			c.Nick = to

			text := fmt.Sprintf("changed nick from %s to %s", from, to)
			buf.writeInfoMessage(text)
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
		text := formatMessage(msg)
		if text == "" {
			continue
		}

		if _, err := file.WriteString(text + "\n"); err != nil {
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
	fmt.Printf("[%s/%s] >> %s\n", c.Name, bufName, line)

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

	case "/l", "/list":
		buf := c.getBuffer(bufName)

		buf.writeInfoMessage("~~ buffers ~~")
		for k, _ := range c.buffers {
			buf.writeInfoMessage(" " + k)
		}

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

	case "/r", "/reconnect":
		if err := c.conn.Close(); err != nil {
			fmt.Printf("failed to close: %+v\n", err)
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

		fmt.Printf("[%s] <-- %+v\n", c.Name, message)
		c.handleMessage(message)
	}
}
