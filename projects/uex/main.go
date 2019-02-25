package main

import (
	"flag"
	"fmt"
	"os"

	"github.com/erik/uex/cmd"
)

const USAGE = `usage: uex [-t SESSION] COMMAND

Commands:

  connect
  bufl
  nickl
  chatl
`

func main() {
	var _ = flag.String("t", "", "Session to attach to.")
	flag.Parse()

	if flag.NArg() != 1 {
		flag.Usage()
		os.Exit(2)
	}

	switch flag.Arg(0) {
	case "connect":
		fmt.Println("choosing connect")
		cmd.Connect()
	case "bufl":
		// TODO:
	case "nickl":
		// TODO:
	case "chat":
		// TODO:
	default:
		flag.Usage()
		os.Exit(2)
	}
}
