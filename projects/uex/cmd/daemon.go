package cmd

import (
	"fmt"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/erik/uex/irc"
)

// TODO: this should support multiple clients
func Daemon() {
	baseDir := "/tmp/uex/"
	cfg := irc.ClientConfiguration{
		Host:  "irc.freenode.net",
		Port:  6697,
		IsTLS: true,

		Nick:     "ep`uex",
		RealName: "erik",

		OnConnect: []string{
			"PING hello",
			"PING world",
		},

		RejoinExisting: true,
	}

	go connectClient(baseDir, cfg)

	// Wait until process receives a SIGINT or SIGTERM, and allow
	// `defer`ed statements to run.
	awaitInterrupt()
}

func connectClient(baseDir string, cfg irc.ClientConfiguration) {
	client := irc.NewClient(baseDir, cfg)

	for {
		err := client.Initialize()
		if err != nil {
			fmt.Printf("connect failed: %+v\n", err)
			goto retry
		}

		// If we exit `RunLoop` cleanly, it was an intentional
		// process exit.
		if err := client.RunLoop(); err == nil {
			break
		}

		fmt.Printf("IRC connection errored: %+v\n", err)
	retry:
		fmt.Println("... sleeping 5 seconds before reconnecting")
		time.Sleep(5 * time.Second)
	}
}

// TODO: Make sure we have a chance to call `defer` statements before
// we shutdown...
func awaitInterrupt() {
	c := make(chan os.Signal)

	signal.Notify(c, os.Interrupt, syscall.SIGTERM)
	signal.Notify(c, os.Interrupt, syscall.SIGINT)

	<-c
	fmt.Println("!!! caught interrupt. starting shutdown.")

	os.Exit(0)
}
