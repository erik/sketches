package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/erik/uex/irc"
)

// Configuration is the on-disk representation of `uex`'s
// daemon configuration.
type Configuration struct {
	Networks  []irc.NetworkConfiguration `json:"networks"`
	Directory string                     `json:"directory"`
}

const (
	defaultConfigPath = "./uex.config.json"
)

func main() {
	path := flag.String("-c", "", "Configuration file to use for connection [default: ./uex.config.json]")
	flag.Parse()

	if *path == "" {
		*path = defaultConfigPath
	}

	config, err := loadConfig(*path)
	if err != nil {
		fmt.Printf("failed to load config: %+v\n", err)
		os.Exit(1)
	}

	runClients(config)
}

func loadConfig(path string) (Configuration, error) {
	var config Configuration

	b, err := ioutil.ReadFile(path)
	if err == nil {
		err = json.Unmarshal(b, &config)
	}

	return config, err
}

func runClients(cfg Configuration) {
	for i := range cfg.Networks {
		go runClient(cfg.Directory, cfg.Networks[i])
	}

	// Wait until process receives a SIGINT or SIGTERM, and allow
	// `defer`ed statements to run.
	awaitInterrupt()
}

func runClient(baseDir string, cfg irc.NetworkConfiguration) {
	client := irc.NewClient(baseDir, cfg)

	for {
		err := client.Connect()
		if err != nil {
			fmt.Printf("connect failed: %+v\n", err)
			goto retry
		}

		// If we exit `Listen` cleanly, it was an intentional
		// process exit.
		if err := client.Listen(); err == nil {
			break
		}

		fmt.Printf("IRC connection errored: %+v\n", err)
	retry:
		fmt.Println("... sleeping 3 seconds before reconnecting")
		time.Sleep(3 * time.Second)
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
