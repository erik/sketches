package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/erik/uex/cmd"
)

const USAGE = `usage: uex COMMAND

Commands:

  daemon
  client [-list]
`

func main() {
	flag.Parse()

	if flag.NArg() != 1 {
		flag.Usage()
		os.Exit(2)
	}

	switch flag.Arg(0) {
	case "daemon":
		cmd.Connect()
	case "client":
		// TODO:
		fmt.Println("client not yet written")
		os.Exit(1)
	default:
		flag.Usage()
		os.Exit(2)
	}
}
