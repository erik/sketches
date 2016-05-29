package daemon

import (
	"log"

	"net"

	"github.com/erik/urk/common"
	"gopkg.in/sorcix/irc.v1"
)

type Daemon struct {
	config common.DaemonConfig
	net    common.NetworkConfig

	conn    *IrcConnection
	clients []chan *irc.Message
}

func New(config *common.Config, net common.NetworkConfig) *Daemon {
	conn := NewIrcConnection(&config.Daemon)

	return &Daemon{
		config:  config.Daemon,
		net:     net,
		conn:    conn,
		clients: make([]chan *irc.Message, 1),
	}
}

func (d *Daemon) Start() {

	log.Printf("Connecting to %s as %s", d.net.Address, d.net.Nick)

	if err := d.conn.Dial(d.net.Address); err != nil {
		log.Fatalf("Failed to connect: %v", err)
	}

	go d.conn.Start()
	go d.acceptClients()

	d.authenticate()
	d.messageLoop()
}

func (d *Daemon) acceptClients() {
	listener, err := net.Listen("tcp", d.config.Listen)
	if err != nil {
		log.Fatal("Failed to listen for connections", err)
	}

	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Fatal("Failed to grab listener", err)
		}

		log.Printf("Handling a client: %v", conn.RemoteAddr())

		go d.handleClientConnection(conn)
	}
}

func (d *Daemon) handleClientConnection(conn net.Conn) {
	c := make(chan *irc.Message, 10)
	d.clients = append(d.clients, c)

	for msg := range c {
		conn.Write(msg.Bytes())
		conn.Write([]byte("\n"))
	}
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
		} else {
			// Just directly pass the message on to each
			// of the the clients
			for _, client := range d.clients {
				client <- msg
			}
		}

		log.Print(msg)
	}
}
