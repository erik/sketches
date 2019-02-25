package cmd

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"time"

	"gopkg.in/sorcix/irc.v2"
)

func prompt(text, defaultVal string) string {
	fmt.Printf("%s (default '%s'): ", text, defaultVal)
	reader := bufio.NewReader(os.Stdin)
	response, err := reader.ReadString('\n')
	response = strings.TrimSpace(response)

	if err != nil {
		os.Exit(1)
	} else if response != "" {
		return response
	}

	return defaultVal
}

func Connect() {
	hostname := prompt("hostname", "irc.freenode.net")
	port := 6667
	nick := "ep`uex"

	for {
		conn, err := createConnection(hostname, port, nick)
		if err != nil {
			fmt.Printf("connect failed: %+v\n", err)
			goto retry
		}

		// If we exit `runLoop` cleanly, it was an intentional process exit.
		if err := runLoop(conn); err == nil {
			break
		}

		fmt.Printf("IRC connection errored: %+v\n", err)
	retry:
		fmt.Println("... sleeping 5 seconds before reconnecting")
		time.Sleep(5 * time.Second)
	}
}

func createConnection(hostname string, port int, nick string) (*irc.Conn, error) {
	server := fmt.Sprintf("%s:%d", hostname, port)
	// TODO: handle TLS connection here as well
	conn, err := irc.Dial(server)
	if err != nil {
		return nil, err
	}

	conn.Encode(&irc.Message{
		Command: "NICK",
		Params:  []string{nick},
	})

	conn.Encode(&irc.Message{
		Command: "USER",
		Params:  []string{nick, "*", "*", "real name"},
	})

	return conn, nil
}

func runLoop(conn *irc.Conn) error {
	for {
		// Methods from both Encoder and Decoder are available
		message, err := conn.Decode()
		if err != nil {
			return err
		}

		fmt.Printf("--> %+v\n", message)
		switch message.Command {

		}
	}
}
