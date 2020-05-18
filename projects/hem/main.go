package main

import (
	"fmt"
        _ "os"

	"github.com/docopt/docopt-go"
)

const USAGE = `hem.

Usage: hem [options] (<FILE> | [-])

  -h --help           Show this help text
  --version           Show version.

  -w, --width=WIDTH   Set maximum line width

See also: 
  fmt(1)
`

func main() {
	arguments, _ := docopt.ParseDoc(USAGE)

	fmt.Println(arguments)
}
