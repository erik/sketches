package irc

import (
	"bufio"
	"crypto/tls"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"
	"unicode"

	"gopkg.in/sorcix/irc.v2"
	"gopkg.in/sorcix/irc.v2/ctcp"
)

const (
	serverBufferName = "$server"
	inputFileName    = "in"
	outputFileName   = "out"
)

// NetworkConfiguration contains the configuration for a connection to
// a single IRC network. Used by Client.
type NetworkConfiguration struct {
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

// Client stores connection information and metadata for a connection
// to an IRC network.
type Client struct {
	NetworkConfiguration

	conn io.ReadWriteCloser
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

func NewClient(baseDir string, cfg NetworkConfiguration) *Client {
	client := &Client{
		NetworkConfiguration: cfg,

		directory: filepath.Join(baseDir, cfg.Name),
		buffers:   make(map[string]buffer),
	}

	return client
}

// Connect opens a TCP connection to the IRC network and sends `NICK`,
// `USER`, and `PASS` commands to authenticate the connection.
func (c *Client) Connect() error {
	if err := c.dial(); err != nil {
		return err
	}

	if c.ServerPass != "" {
		c.send("PASS", c.ServerPass)
	}

	realName := c.RealName
	if realName == "" {
		realName = c.Nick
	}

	c.send("NICK", c.Nick)
	c.send("USER", c.Nick, "*", "*", realName)

	return nil
}

// Listen loops through all IRC messages sent to the client as long as
// the connection remains open, dispatching to handlers. Will return
// an error if the connection is interrupted, or an unparseable
// message is returned.
func (c *Client) Listen() error {
	scanner := bufio.NewScanner(c.conn)
	for scanner.Scan() {
		line := scanner.Text()
		message := irc.ParseMessage(line)
		if message == nil {
			fmt.Printf("[%s] <-- invalid message: %s\n", c.Name, line)
			continue
		}

		fmt.Printf("[%s] <-- %+v\n", c.Name, message)
		c.handleMessage(message)
	}

	return scanner.Err()
}

// dial handles the TCP/TLS details of connecting to an IRC network.
func (c *Client) dial() error {
	c.serverBuffer().writeInfoMessage("connecting ...")

	server := fmt.Sprintf("%s:%d", c.Host, c.Port)

	conn, err := net.Dial("tcp", server)
	if err != nil {
		return err
	}

	tcpc := conn.(*net.TCPConn)
	if err = tcpc.SetKeepAlive(true); err != nil {
		return err
	}
	if err = tcpc.SetKeepAlivePeriod(5 * time.Minute); err != nil {
		return err
	}

	if c.IsTLS {
		conn = tls.Client(conn, &tls.Config{
			ServerName:         c.Host,
			InsecureSkipVerify: c.SkipCertificateCheck,
		})
	}

	c.conn = conn
	return nil
}

func (c *Client) send(cmd string, params ...string) {
	msg := &irc.Message{
		Command: cmd,
		Params:  params,
	}

	c.sendRaw(msg.Bytes())
}

func (c *Client) sendRaw(msg []byte) {
	line := append(msg, '\r', '\n')

	if _, err := c.conn.Write(line); err != nil {
		log.Printf("Failed to write... %+v\n", err)
	}

	fmt.Printf("[%s] --> %+v\n", c.Name, string(msg))
}

// listExistingChannels returns a list of channel names that were
// found in the client's output directory.
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

func (c *Client) handleMessage(msg *irc.Message) {
	buf := c.getBuffer(serverBufferName)

	switch msg.Command {
	case irc.RPL_WELCOME:
		for _, msg := range c.OnConnect {
			c.sendRaw([]byte(msg))
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

		if ts, err := strconv.ParseInt(s[1], 10, 64); err == nil {
			delta := time.Duration(time.Now().UnixNano()-ts) / time.Millisecond
			text := fmt.Sprintf("PONG from %s: %d ms", msg.Params[0], delta)

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

		c.getBuffer(target).topic = topic

	case irc.ERR_NICKNAMEINUSE:
		c.Nick = c.Nick + "`"
		fmt.Printf("Nick in use, trying '%s'\n", c.Nick)
		c.send("NICK", c.Nick)
	}

	if buf != nil {
		buf.ch <- msg
	}
}

func (c *Client) serverBuffer() *buffer {
	return c.getBuffer(serverBufferName)
}

func (c *Client) getBuffer(name string) *buffer {
	c.mux.Lock()
	defer c.mux.Unlock()

	name = normalizeBufferName(name)

	// Sent early on, at least by freenode.
	if name == "*" {
		name = serverBufferName
	}

	if buf, exists := c.buffers[name]; exists {
		return &buf
	}

	path := c.directory

	// We want to write `in`, `out` top level for the server, and
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

func (c *Client) handleInputLine(bufName, line string) {
	fmt.Printf("[%s/%s] >> %s\n", c.Name, bufName, line)

	cmd, rest := splitInputCommand(bufName, line)

	switch cmd {
	case "/m", "/msg":
		s := strings.SplitN(rest, " ", 2)
		if len(s) != 2 {
			c.serverBuffer().writeInfoMessage("expected: /msg TARGET MESSAGE")
			return
		} else if s[0] == serverBufferName {
			c.serverBuffer().writeInfoMessage("can't PRIVMSG a server.")
			return
		}

		buf := c.getBuffer(s[0])

		// TODO: pull this out into simple `buf.AddMessage` or so
		buf.ch <- &irc.Message{
			Prefix:  &irc.Prefix{Name: c.Nick},
			Command: irc.PRIVMSG,
			Params:  []string{s[1]},
		}

		c.send("PRIVMSG", s[0], s[1])

	case "/me":
		action := ctcp.Action(rest)

		buf := c.getBuffer(bufName)
		buf.ch <- &irc.Message{
			Prefix:  &irc.Prefix{Name: c.Nick},
			Command: irc.PRIVMSG,
			Params:  []string{action},
		}

		c.send("PRIVMSG", bufName, action)

	case "/j", "/join":
		if !isChannel(rest) {
			c.getBuffer(bufName).writeInfoMessage("expected: /join TARGET")
			return
		}
		c.send("JOIN", rest)

	case "/l", "/list":
		buf := c.getBuffer(bufName)

		buf.writeInfoMessage("~~ buffers ~~")
		for k := range c.buffers {
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
		c.serverBuffer().writeInfoMessage("... disconnecting")
		if err := c.conn.Close(); err != nil {
			fmt.Printf("failed to close: %+v\n", err)
		}

	default:
		text := fmt.Sprintf("Unknown command: %s %s", cmd, rest)
		c.getBuffer(bufName).writeInfoMessage(text)
	}
}

func (b *buffer) writeInfoMessage(msg string) {
	b.ch <- &irc.Message{
		Prefix:  &irc.Prefix{Name: "uex"},
		Command: "*",
		Params:  []string{msg},
	}
}

func (b *buffer) outputHandler() {
	name := filepath.Join(b.path, outputFileName)
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
	name := filepath.Join(b.path, inputFileName)
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

// normalizeBufferName strips out illegal characters and standardizes
// naming so that it's safe to write as a directory name to the file
// system.
func normalizeBufferName(buffer string) string {
	return strings.Map(func(ch rune) rune {
		if unicode.IsLetter(ch) || unicode.IsNumber(ch) {
			return unicode.ToLower(ch)
		} else if strings.ContainsRune(".#&+!-", ch) {
			return ch
		}

		return '_'
	}, buffer)
}

// splitInputCommand returns `(command, param)` for a line of input
// received from the user. If no command is explicitly specified,
// assume "/msg" (i.e. PRIVMSG).
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
