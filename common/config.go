package common

import (
	"io/ioutil"
	"log"

	"github.com/BurntSushi/toml"
)

type DaemonConfig struct {
	Listen string
	LogDir string
}

type NetworkConfig struct {
	Address  string
	Nick     string
	User     string
	Channels []string
}

type Config struct {
	Daemon  DaemonConfig
	Network map[string]NetworkConfig
	Client  struct{}
}

const defaultConfig = `
[daemon]
listen = ":5567"
logdir = "/tmp/urklog/"

[network.freenode]
address = "irc.freenode.net:6667"
nick = "ErikBot"
user = "Erik"
`

func LoadConfig(fileName string) (*Config, error) {
	var config Config
	var configString string

	if fileName == "" {
		log.Println("Falling back to default configuration")
		configString = defaultConfig
	} else {
		configBytes, err := ioutil.ReadFile(fileName)
		if err != nil {
			return nil, err
		}

		configString = string(configBytes)
	}

	if _, err := toml.Decode(configString, &config); err != nil {
		return nil, err
	}

	return &config, nil
}
