#![allow(dead_code, unused_variables)]

use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Source {
    nick: String,
    ident: Option<String>,
    host: Option<String>,
    is_server: bool,
}

//  <prefix>   ::= <nick> [ '!' <ident>  [ '@' <host> ] ]
impl Source {
    fn parse(s: &str) -> Option<Source> {
        if s.is_empty() {
            return None;
        }

        let (nick, rest) = if let Some(index) = s.find('!') {
            (&s[..index], Some(&s[index + 1..]))
        } else {
            (s, None)
        };

        let (ident, host) = rest
            .map(|r| {
                if let Some(index) = r.find('@') {
                    (Some(&r[..index]), Some(&r[index + 1..]))
                } else {
                    (rest, None)
                }
            })
            .unwrap_or((None, None));

        Some(Source {
            nick: nick.trim_start_matches(":").to_string(),
            ident: ident.map(str::to_string),
            host: host.map(str::to_string),
            is_server: nick.starts_with(':'),
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum MessageKind {
    Away,
    Capability,
    Error,
    Invite,
    Join,
    Kick,
    Mode,
    Nick,
    Notice,
    Numeric(u16),
    Part,
    Pass,
    Ping,
    Pong,
    Privmsg,
    Quit,
    Topic,
    User,
    Unknown,
    Wallops,
}

impl MessageKind {
    pub fn from(msg: &RawMessage) -> MessageKind {
        if let Ok(code) = msg.command.parse::<u16>() {
            return MessageKind::Numeric(code);
        }

        match msg.command.as_str() {
            "CAP" => Self::Capability,
            "JOIN" => Self::Join,
            "KICK" => Self::Kick,
            "NOTICE" => Self::Notice,
            "PART" => Self::Part,
            "PASS" => Self::Pass,
            "PING" => Self::Ping,
            "PONG" => Self::Pong,
            "PRIVMSG" => Self::Privmsg,
            "USER" => Self::User,

            // TODO: Map remaining types
            _ => Self::Unknown,
        }
    }
}

type MessageTags = HashMap<String, MessageTag>;

#[derive(Debug, PartialEq)]
pub struct MessageTag {
    key: String,
    value: Option<String>,
}

#[derive(Debug, PartialEq)]
pub struct RawMessage {
    pub source: Option<Source>,
    pub command: String,
    params: Vec<String>,

    pub timestamp: u64, // TODO: datetime
    pub tags: MessageTags,
}

/// Split `msg` at first space character or end of word if no spaces remain.
fn split_message(msg: &str) -> Option<(&str, &str)> {
    if msg.len() == 0 {
        return None;
    }

    match msg.find(' ') {
        Some(index) => Some((&msg[..index], &msg[index + 1..])),

        None => Some((msg, "")),
    }
}
//  <message>  ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
//  <prefix>   ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
//  <command>  ::= <letter> { <letter> } | <number> <number> <number>
//  <SPACE>    ::= ' ' { ' ' }
//  <params>   ::= <SPACE> [ ':' <trailing> | <middle> <params> ]
//  <middle>   ::= <Any *non-empty* sequence of octets not including SPACE
//                 or NUL or CR or LF, the first of which may not be ':'>
//  <trailing> ::= <Any, possibly *empty*, sequence of octets not including
//                   NUL or CR or LF>
impl RawMessage {
    /// Expects that any trailing CR, LF have been removed.
    pub fn parse(line: &str) -> Option<Self> {
        let mut line: &str = line;
        let mut msg: Self;

        let tags = if line.starts_with('@') {
            let (tags, rest) = split_message(line)?;
            line = rest;
            // TODO: parse tags
            HashMap::new()
        } else {
            HashMap::new()
        };

        let nick = if line.starts_with(':') {
            let (nick, rest) = split_message(line)?;
            line = rest;
            Some(Source::parse(nick)?)
        } else {
            None
        };

        let (cmd, mut line) = split_message(line)?;
        // line = rest;

        let mut params: Vec<String> = Vec::new();
        while let Some((param, rest)) = split_message(line) {
            // Trailing param, this will be the end, regardless of
            // remaining whitespace.
            if param.starts_with(':') {
                params.push(line[1..].to_string());
                break;
            } else {
                params.push(param.to_string());
            }

            line = rest;
        }

        Some(Self {
            source: nick,
            command: cmd.to_string(),
            params: params,
            tags: tags,

            // TODO: pull this out of the tags, if possible
            timestamp: 0,
        })
    }

    pub fn param(&self, i: usize) -> Option<&str> {
        self.params.get(i).map(String::as_ref)
    }
}

/// IRC capabilities we support
#[derive(Hash, PartialEq, Eq)]
pub enum Capability {
    ServerTime,
}

impl Capability {
    pub fn from(s: &str) -> Option<Self> {
        match s {
            "server-time" => Some(Self::ServerTime),
            _ => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_source() {
        assert_eq!(
            Source::parse("nick!~ident@host").unwrap(),
            Source {
                nick: "nick".to_string(),
                ident: Some("~ident".to_string()),
                host: Some("host".to_string()),
                is_server: false,
            }
        );

        assert_eq!(
            Source::parse("nick!~user").unwrap(),
            Source {
                nick: "nick".to_string(),
                ident: Some("~user".to_string()),
                host: None,
                is_server: false,
            }
        );

        assert_eq!(
            Source::parse("nick").unwrap(),
            Source {
                nick: "nick".to_string(),
                ident: None,
                host: None,
                is_server: false,
            }
        );

        assert_eq!(
            Source::parse(":irc.server!~ident@host").unwrap(),
            Source {
                nick: "irc.server".to_string(),
                ident: Some("~ident".to_string()),
                host: Some("host".to_string()),
                is_server: true,
            }
        );
    }

    #[test]
    fn test_parse_messages() {
        let line = "@foo=bar :server 001 abc :hello, world";
        let msg = RawMessage::parse(line).expect("failed to parse");

        assert_eq!(msg.command, "001");
        assert_eq!(msg.source.unwrap().nick, "server");
        assert_eq!(msg.params, vec!["abc", "hello, world"]);
        // TODO: wire up assert
    }

    #[test]
    fn test_split_message() {
        assert_eq!(split_message("foo bar baz"), Some(("foo", "bar baz")));
        assert_eq!(split_message("foo bar"), Some(("foo", "bar")));
        assert_eq!(split_message("foo"), Some(("foo", "")));
        assert_eq!(split_message(""), None);
        assert_eq!(split_message("trailing "), Some(("trailing", "")));
        assert_eq!(split_message(" leading"), Some(("", "leading")));
    }
}
