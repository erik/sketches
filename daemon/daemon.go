package daemon

import (
	"io"
	"log"

	"gopkg.in/sorcix/irc.v1"
)

type DaemonConfig struct {
	Listen  string
	LogDir  string
	Network map[string]NetworkConfig
}

type NetworkConfig struct {
	Address  string
	Nick     string
	RealName string
}

type daemon struct {
	config  DaemonConfig
	clients []io.ReadWriter
}

func Start(config DaemonConfig) {
	log.Println("Starting in daemon mode")

	daemon := daemon{
		config: config,
	}

	for _, net := range config.Network {
		log.Printf("Connecting to %s as %s", net.Address, net.Nick)

		conn, err := irc.Dial(net.Address)

		if err != nil {
			log.Fatalf("Failed to connect: %v", err)
		}

		c := make(chan *irc.Message, 10)

		go daemon.handleConnection(c, conn)
		go daemon.messageLoop(c)

	}

	// Our work is done, take a nap.
	select {}
}

func (d *daemon) messageLoop(c chan *irc.Message) {
	for msg := range c {
		if msg.Command == irc.PING {

		}

		log.Print(msg)
	}
}

func (d *daemon) handleConnection(c chan *irc.Message, conn *irc.Conn) {
	for {
		message, err := conn.Decode()

		if err != nil {
			log.Fatal(err)
		}

		c <- message
	}
}
