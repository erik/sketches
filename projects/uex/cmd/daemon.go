package cmd

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"time"

	"github.com/erik/uex/irc"
)

func Daemon() {
	hostname := "irc.freenode.net"
	port := 6667
	nick := "ep`uex"

	baseDir, err := ioutil.TempDir("", hostname)
	if err != nil {
		log.Fatal(err)
	}
	defer os.RemoveAll(baseDir)

	fmt.Printf("==> Output to %s\n", baseDir)

	for {
		client, err := irc.NewClient(hostname, port, baseDir)
		if err != nil {
			fmt.Printf("connect failed: %+v\n", err)
			goto retry
		}

		client.Initialize(nick, "")

		// If we exit `RunLoop` cleanly, it was an intentional process exit.
		if err := client.RunLoop(); err == nil {
			break
		}

		fmt.Printf("IRC connection errored: %+v\n", err)
	retry:
		fmt.Println("... sleeping 5 seconds before reconnecting")
		time.Sleep(5 * time.Second)
	}
}
