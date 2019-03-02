package cmd

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/signal"
	"runtime"
	"syscall"
	"time"

	"github.com/erik/uex/irc"
)

// TODO: this should support multiple clients
func Daemon() {
	// TODO: shouldn't need to do this.
	baseDir, err := ioutil.TempDir("", "")
	if err != nil {
		log.Fatal(err)
	}

	defer func() {
		fmt.Println("cleaning up!")
		if err := os.RemoveAll(baseDir); err != nil {
			fmt.Printf("CLEANUP FAILED: %+v\n", err)
		}
		fmt.Println("done!")
	}()

	cfg := irc.Config{
		Host:  "irc.freenode.net",
		Port:  6697,
		IsTLS: true,

		Nick:     "ep`uex",
		RealName: "erik",
	}

	fmt.Printf("==> Output to %s\n", baseDir)

	go connectClient(cfg)

	// Wait until process receives a SIGINT or SIGTERM, and allow
	// `defer`ed statements to run.
	awaitInterrupt()
}

func connectClient(cfg irc.Config) {
	for {
		client, err := irc.NewClient(cfg)
		if err != nil {
			fmt.Printf("connect failed: %+v\n", err)
			goto retry
		}

		client.Initialize()

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

// Attempt to make sure we have a chance to call all `defer`
// statements before exiting the process.
//
// FIXME: This is super jank and doesn't work as expected. Only kills
// CURRENT goroutine (main), but leaves everything else running.
func awaitInterrupt() {
	c := make(chan os.Signal)

	signal.Notify(c, os.Interrupt, syscall.SIGTERM)
	signal.Notify(c, os.Interrupt, syscall.SIGINT)

	<-c
	fmt.Println("!!! caught interrupt. starting shutdown.")

	go func() {
		fmt.Println("... 5 seconds please")
		time.Sleep(5 * time.Second)
		os.Exit(0)
	}()

	runtime.Goexit()
}
