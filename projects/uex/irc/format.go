package irc

import (
	"fmt"
	"hash/fnv"
	"time"

	"gopkg.in/sorcix/irc.v2"
)

const (
	timestampFormat = "15:04:05"
	alertSender     = "~!~"
	resetColors     = "\x1B[0m"
)

// TODO: Maybe use 256 color mode? \x1b[38;5;NUMBERm
var nickColors = []string{
	"\x1B[30m", "\x1B[30;1m", "\x1B[30;2m",
	"\x1B[31m", "\x1B[31;1m", "\x1B[31;2m",
	"\x1B[32m", "\x1B[32;1m", "\x1B[32;2m",
	"\x1B[33m", "\x1B[33;1m", "\x1B[33;2m",
	"\x1B[34m", "\x1B[34;1m", "\x1B[34;2m",
	"\x1B[35m", "\x1B[35;1m", "\x1B[35;2m",
	"\x1B[36m", "\x1B[36;1m", "\x1B[36;2m",
}

func colorizeNick(nick string) string {
	h := fnv.New32a()
	h.Write([]byte(nick))

	// Don't want negative indices, so do the mod on unsigned numbers.
	i := int(h.Sum32() % uint32(len(nickColors)))
	col := nickColors[i]

	return fmt.Sprintf("%s%15s%s", col, nick, resetColors)
}

func formatMessage(m *irc.Message) string {
	ts := time.Now().Format(timestampFormat)
	sender := alertSender
	line := fmt.Sprintf("%s %s", m.Command, m.Trailing())

	switch m.Command {
	case irc.PRIVMSG, irc.NOTICE:
		sender = m.Prefix.Name
		line = m.Trailing()

	case irc.RPL_TOPIC:
		sender = m.Prefix.Name
		line = fmt.Sprintf("%s: topic is \"%s\"", m.Params[1], m.Trailing())

	case irc.JOIN:
		nick := m.Prefix.Name
		line = fmt.Sprintf("%s joined", nick)

	case irc.PART:
		nick := m.Prefix.Name
		line = fmt.Sprintf("%s left: %s", nick, m.Trailing())

	case irc.PING, irc.RPL_TOPICWHOTIME,
		irc.RPL_NAMREPLY, irc.RPL_ENDOFNAMES:
		// These are the skippable ones.
		return ""
	}

	return fmt.Sprintf("%s %s %s", ts, colorizeNick(sender), line)

}
