package main

import (
	"flag"
	"log"

	"github.com/erik/urk/client"
	"github.com/erik/urk/common"
	"github.com/erik/urk/daemon"
)

var (
	daemonMode = flag.Bool("daemon", false, "toggles whether this is a daemon")
	configFile = flag.String("config", "", "path to config file")
)

func main() {
	flag.Parse()

	config, err := common.LoadConfig(*configFile)

	if err != nil {
		log.Fatal(err)
	}

	log.Printf("%v\n", config)

	if *daemonMode {
		log.Println("Starting in daemon mode")

		for _, net := range config.Network {
			d := daemon.New(config, net)
			go d.Start()
		}

	} else {
		client.Start()
	}

	// Our work is done, take a nap.
	select {}
}
