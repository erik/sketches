package daemon

import (
	"bufio"
	"io"
	"log"
	"net"

	"gopkg.in/sorcix/irc.v1"

	"github.com/erik/urk/common"
)

type connectionInner struct {
	socket io.ReadWriteCloser
	reader *bufio.Reader
}

type IrcConnection struct {
	Data chan *irc.Message

	config  *common.DaemonConfig
	conn    *connectionInner
	backlog *Backlog
}

func NewIrcConnection(config *common.DaemonConfig) *IrcConnection {
	return &IrcConnection{
		config: config,
		Data:   make(chan *irc.Message, 25),
	}
}

func (c *IrcConnection) Dial(address string) error {
	conn, err := net.Dial("tcp", address)
	if err != nil {
		return err
	}

	c.conn = &connectionInner{
		socket: conn,
		reader: bufio.NewReader(conn),
	}

	c.backlog = NewBacklog(c.config.LogDir, address)

	return nil
}

func (c *IrcConnection) sendRaw(msg []byte) error {
	if _, err := c.conn.socket.Write(msg); err != nil {
		return err
	}

	return nil
}

func (c *IrcConnection) Send(msg *irc.Message) error {
	if err := c.backlog.AddMessage(msg); err != nil {
		return err
	}

	return c.sendRaw(msg.Bytes())
}

func (c *IrcConnection) Start() {
	for {
		line, err := c.conn.reader.ReadString('\n')

		if err != nil {
			log.Fatal(err)
		}

		msg := irc.ParseMessage(line)

		if msg == nil {
			log.Printf("Message failed to parse: %s\n", line)
			continue
		}

		c.Data <- msg
	}
}
