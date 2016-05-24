package daemon

import (
	"io"
	"log"

	"github.com/erik/urk/common"
	"gopkg.in/sorcix/irc.v1"
)

type Daemon struct {
	config common.DaemonConfig
	net    common.NetworkConfig

	conn    *IrcConnection
	clients []io.ReadWriteCloser
}

func New(config *common.Config, net common.NetworkConfig) *Daemon {
	conn := NewIrcConnection(&config.Daemon)

	return &Daemon{
		config: config.Daemon,
		net:    net,
		conn:   conn,

		clients: make([]io.ReadWriteCloser, 0),
	}
}

func (d *Daemon) Start() {

	log.Printf("Connecting to %s as %s", d.net.Address, d.net.Nick)

	if err := d.conn.Dial(d.net.Address); err != nil {
		log.Fatalf("Failed to connect: %v", err)
	}

	go d.conn.Start()

	d.authenticate()
	d.messageLoop()
}

func (d *Daemon) authenticate() {
	d.conn.Send(&irc.Message{
		Command: irc.NICK,
		Params:  []string{d.net.Nick},
	})

	d.conn.Send(&irc.Message{
		Command:  irc.USER,
		Params:   []string{d.net.User, "0", "*"},
		Trailing: d.net.User,
	})

	d.conn.Send(&irc.Message{
		Command: irc.JOIN,
		Params:  []string{"#bot-test-1555"},
	})
}

func (d *Daemon) messageLoop() {
	for msg := range d.conn.Data {
		if msg.Command == irc.PING {
			log.Println("SENDING PONG")

			d.conn.Send(&irc.Message{
				Command:  irc.PONG,
				Params:   msg.Params,
				Trailing: msg.Trailing,
			})
		}

		log.Print(msg)
	}
}
