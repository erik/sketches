package irc

import (
	"fmt"
	"time"

	"gopkg.in/sorcix/irc.v2"
)

const (
	timestampFormat = "15:04:05"

	alertSender = "~!~"
)

func formatMessage(m *irc.Message) string {
	ts := time.Now().Format(timestampFormat)
	sender := alertSender
	line := fmt.Sprintf("%s %s", m.Command, m.Trailing())

	switch m.Command {
	case irc.PRIVMSG, irc.NOTICE:
		sender = m.Prefix.User
		line = m.Trailing()

	case irc.RPL_TOPIC:
		sender = m.Prefix.User
		line = fmt.Sprintf("sets topic to \"%s\"", m.Trailing())

	case irc.PING, irc.RPL_TOPICWHOTIME,
		irc.RPL_NAMREPLY, irc.RPL_ENDOFNAMES:
		// These are the skippable ones.
		return ""
	}

	return fmt.Sprintf("%s %15s %s", ts, sender, line)

}
