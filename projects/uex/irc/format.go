package irc

import (
	"fmt"
	"hash/fnv"
	"regexp"
	"time"

	"gopkg.in/sorcix/irc.v2"
	"gopkg.in/sorcix/irc.v2/ctcp"
)

const (
	timestampFormat = "15:04:05"
	alertSender     = "~!~"
	resetColors     = "\x1B[0m"
)

// TODO: Maybe use 256 color mode? \x1b[38;5;NUMBERm
var nickColors = []string{
	"", "\x1B[30;1m", "\x1B[30;2m",
	"\x1B[31m", "\x1B[31;1m", "\x1B[31;2m",
	"\x1B[32m", "\x1B[32;1m", "\x1B[32;2m",
	"\x1B[33m", "\x1B[33;1m", "\x1B[33;2m",
	"\x1B[34m", "\x1B[34;1m", "\x1B[34;2m",
	"\x1B[35m", "\x1B[35;1m", "\x1B[35;2m",
	"\x1B[36m", "\x1B[36;1m", "\x1B[36;2m",
}

// colorizeNick takes the given nick and wraps it with ANSI terminal
// codes to colorize it. The nick is hashed to make sure that the
// coloring is stable.
func colorizeNick(nick string) string {
	h := fnv.New32a()
	h.Write([]byte(nick))

	// Don't want negative indices, so do the mod on unsigned numbers.
	i := int(h.Sum32() % uint32(len(nickColors)))
	col := nickColors[i]

	return fmt.Sprintf("%s%15s%s", col, nick, resetColors)
}

// https://github.com/myano/jenni/wiki/IRC-String-Formatting
// https://misc.flogisoft.com/bash/tip_colors_and_formatting
var ircToTerm = map[string]string{
	"00": "37",   // white
	"01": "30",   // black
	"02": "34",   // blue (navy)
	"03": "32",   // green
	"04": "31",   // red
	"05": "35",   // brown (maroon)
	"06": "35;1", // purple
	"07": "33",   // orange (olive)
	"08": "33;1", // yellow
	"09": "32;1", // light green (lime)
	"10": "34;1", // teal (a green/blue cyan)
	"11": "34;1", // light cyan (cyan / aqua)
	"12": "34;1", // light blue (royal)
	"13": "37;1", // pink (light purple / fuchsia)
	"14": "37;1", // grey
	"15": "37;1", // light grey (silver)
}

// stylizeLine converts IRC formatting codes to ANSI terminal codes.
func stylizeLine(line string) string {
	r := regexp.MustCompile("[\x02\x1D\x0F\x1F]|(\x03(?:\\d\\d(?:,\\d\\d)?)?)")
	line = r.ReplaceAllStringFunc(line, func(m string) string {
		code := "0"
		switch m[0] {
		// Bold
		case '\x02':
			code = "1"

		// Colored text
		case '\x03':
			// Ignore background codes
			if len(m) >= 3 {
				code, _ = ircToTerm[m[1:3]]
			}

		// Italic text
		case '\x1D':
			code = "4"

		// Underlined text
		case '\x1F':
			code = "4"

		// Swap background and foreground colors ("reverse video")
		case '\x16':
			code = "7"

		// reset all formatting
		case '\x0F':
			code = "0"
		}

		return fmt.Sprintf("\x1B[%sm", code)
	})

	return line + "\x1B[0m"
}

// formatMessage returns a string fit for printing to a terminal.
func formatMessage(m *irc.Message) string {
	ts := time.Now().Format(timestampFormat)
	sender := alertSender
	line := fmt.Sprintf("%s %s", m.Command, m.Trailing())

	switch m.Command {
	case irc.PRIVMSG, irc.NOTICE:
		sender = m.Prefix.Name
		line = m.Trailing()

		// Handle CTCP ACTIONS
		if tag, text, ok := ctcp.Decode(m.Trailing()); ok && tag == "ACTION" {
			sender = alertSender
			line = fmt.Sprintf("%s %s", m.Prefix.Name, text)
		}

		line = stylizeLine(line)

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
