package main

import (
	"flag"
	"log"

	"github.com/BurntSushi/toml"
	"github.com/erik/urk/client"
	"github.com/erik/urk/daemon"
)

type Config struct {
	Daemon daemon.DaemonConfig

	Client struct {
	}
}

const defaultConfig = `
[daemon]
listen = ":5567"
logdir = "/tmp/urklog/"

[daemon.network.freenode]
address = "irc.freenode.net:6667"
nick = "ErikBot"
realname = "Erik Bot"
`

var daemonMode = flag.Bool("daemon", false, "toggles whether this is a daemon")
var configFile = flag.String("config", "", "path to config file")

func main() {
	flag.Parse()

	var config Config

	if _, err := toml.Decode(defaultConfig, &config); err != nil {
		log.Fatal(err)
	}

	log.Printf("%v\n", config)

	if *daemonMode {
		daemon.Start(config.Daemon)
	} else {
		client.Start()
	}
}
