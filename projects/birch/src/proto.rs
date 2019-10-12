#![allow(dead_code, unused_variables)]

use std::collections::HashMap;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Source {
    pub nick: String,
    pub ident: Option<String>,
    pub host: Option<String>,
    pub is_server: bool,
}

impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_server {
            write!(f, ":")?;
        }
        write!(f, "{}", self.nick)?;
        if let Some(ref ident) = self.ident {
            write!(f, "!{}", ident)?;
        }
        if let Some(ref host) = self.host {
            write!(f, "@{}", host)?;
        }

        Ok(())
    }
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
            nick: nick.trim_start_matches(':').to_string(),
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
            "MODE" => Self::Mode,
            "NOTICE" => Self::Notice,
            "PART" => Self::Part,
            "PASS" => Self::Pass,
            "PING" => Self::Ping,
            "PONG" => Self::Pong,
            "PRIVMSG" => Self::Privmsg,
            "USER" => Self::User,
            "ERROR" => Self::Error,
            // TODO: Map remaining types
            _ => Self::Unknown,
        }
    }
}

// <tags>          ::= '@' <tag> [';' <tag>]*
// <tag>           ::= <key> ['=' <escaped value>]
// <key>           ::= [ <vendor> '/' ] <sequence of letters, digits, hyphens (`-`)>
// <escaped value> ::= <sequence of any characters except NUL, CR, LF, semicolon (`;`) and SPACE>
// <vendor>        ::= <host>
#[derive(Debug, PartialEq)]
pub struct Tags(HashMap<String, Tag>);

impl Tags {
    pub fn empty() -> Self {
        Self(HashMap::new())
    }

    pub fn parse(s: &str) -> Option<Self> {
        if !s.starts_with('@') {
            return None;
        }

        let map = s[1..]
            .split(';')
            .map(Tag::parse)
            .map(|t| (t.key.clone(), t))
            .collect();

        Some(Tags(map))
    }
}

#[derive(Debug, PartialEq)]
pub struct Tag {
    pub key: String,
    pub value: Option<String>,
}

impl Tag {
    pub fn parse(s: &str) -> Self {
        if let Some(idx) = s.find('=') {
            Tag {
                key: s[..idx].to_string(),
                value: Some(s[idx + 1..].to_string()),
            }
        } else {
            Tag {
                key: s.to_string(),
                value: None,
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct RawMessage {
    pub source: Option<Source>,
    pub command: String,
    params: Vec<String>,

    pub timestamp: u64, // TODO: datetime
    pub tags: Tags,
}

/// Split `msg` at first space character or end of word if no spaces remain.
fn split_message(msg: &str) -> Option<(&str, &str)> {
    if msg.is_empty() {
        return None;
    }

    match msg.find(' ') {
        Some(index) => Some((&msg[..index], &msg[index + 1..])),
        None => Some((msg, "")),
    }
}

impl RawMessage {
    pub fn new(command: &str, params: &[&str]) -> RawMessage {
        RawMessage {
            command: command.to_string(),
            params: params.iter().map(|s| s.to_string()).collect(),

            source: None,
            timestamp: 0,
            tags: Tags::empty(),
        }
    }

    /// Expects that any trailing CR, LF have been removed.
    ///
    ///  <message>  ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
    ///  <prefix>   ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
    ///  <command>  ::= <letter> { <letter> } | <number> <number> <number>
    ///  <SPACE>    ::= ' ' { ' ' }
    ///  <params>   ::= <SPACE> [ ':' <trailing> | <middle> <params> ]
    ///  <middle>   ::= <Any *non-empty* sequence of octets not including SPACE
    ///                 or NUL or CR or LF, the first of which may not be ':'>
    ///  <trailing> ::= <Any, possibly *empty*, sequence of octets not including
    ///                   NUL or CR or LF>
    pub fn parse(line: &str) -> Option<Self> {
        let mut line: &str = line;
        let mut msg: Self;

        let tags = if line.starts_with('@') {
            let (tags, rest) = split_message(line)?;
            line = rest;
            Tags::parse(tags)?
        } else {
            Tags::empty()
        };

        let nick = if line.starts_with(':') {
            let (nick, rest) = split_message(line)?;
            line = rest;
            Some(Source::parse(nick)?)
        } else {
            None
        };

        let (cmd, mut line) = split_message(line)?;

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
            params,
            tags,

            // TODO: pull this out of the tags, if possible
            timestamp: 0,
        })
    }

    pub fn param(&self, i: usize) -> Option<&str> {
        self.params.get(i).map(String::as_ref)
    }

    pub fn trailing(&self) -> Option<&str> {
        self.params.last().map(String::as_ref)
    }
}

impl fmt::Display for RawMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TOOD: tags

        if let Some(source) = &self.source {
            write!(f, "{} ", source)?;
        }

        write!(f, "{}", self.command)?;

        for (i, param) in self.params.iter().enumerate() {
            // Last is a special case
            if param.contains(' ') {
                write!(f, " :{}", param)?;
            } else {
                write!(f, " {}", param)?;
            }
        }

        Ok(())
    }
}

/// IRC capabilities we support
#[derive(Debug, Hash, PartialEq, Eq)]
pub enum Capability {
    ServerTime,
    Sasl(Vec<String>),
}

impl Capability {
    pub fn from(s: &str) -> Option<Self> {
        let split: Vec<&str> = s.splitn(2, '=').collect();

        let cap = *split.get(0)?;
        let args = split.get(1).unwrap_or(&"").split(',');

        match cap {
            "server-time" => Some(Self::ServerTime),
            "sasl" => {
                let set = args.map(str::to_uppercase).collect();
                Some(Self::Sasl(set))
            }
            _ => None,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ModeSet(HashMap<char, bool>);

impl ModeSet {
    pub fn empty() -> Self {
        Self(HashMap::new())
    }

    pub fn apply(&self, s: &str) -> Option<Self> {
        let mut value: Option<bool> = None;
        let mut copy = self.clone();

        for ch in s.chars() {
            match ch {
                '+' => value = Some(true),
                '-' => value = Some(false),
                _ => {
                    if let Some(value) = value {
                        copy.0.insert(ch, value);
                    } else {
                        // Invalid string format, bail early
                        return None;
                    }
                }
            }
        }

        Some(copy)
    }

    pub fn contains(&self, m: char) -> bool {
        self.0.get(&m).map(bool::clone).unwrap_or(false)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_mode_string() {
        let modes = ModeSet::empty().apply("-bB+xy");
        let expected = [('b', false), ('B', false), ('x', true), ('y', true)]
            .iter()
            .cloned()
            .collect();

        assert_eq!(modes, Some(ModeSet(expected)));
    }

    #[test]
    fn test_parse_caps() {
        assert_eq!(
            Capability::from("server-time"),
            Some(Capability::ServerTime)
        );

        assert_eq!(
            Capability::from("sasl=plain,EXTERNAL"),
            Some(Capability::Sasl(vec![
                "PLAIN".to_string(),
                "EXTERNAL".to_string()
            ]))
        );
    }

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
    fn test_display_source() {
        let cases = vec![
            "nick",
            "nick!~ident",
            "nick!~ident@host",
            ":irc.server!~ident@host",
        ];

        for case in cases {
            let round_trip = format!("{}", Source::parse(case).unwrap());
            assert_eq!(round_trip, case);
        }
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

    #[test]
    fn test_parse_tag() {
        let cases = vec![
            (
                "key",
                Tag {
                    key: "key".to_string(),
                    value: None,
                },
            ),
            (
                "key=",
                Tag {
                    key: "key".to_string(),
                    value: Some("".to_string()),
                },
            ),
            (
                "key=val",
                Tag {
                    key: "key".to_string(),
                    value: Some("val".to_string()),
                },
            ),
        ];

        for (input, expected) in cases {
            assert_eq!(Tag::parse(input), expected);
        }
    }

    #[test]
    fn test_parse_tags() {
        let tags = Tags::parse("@foo=bar;baz=;quux");
        let expected = vec![
            (
                "foo".to_string(),
                Tag {
                    key: "foo".to_string(),
                    value: Some("bar".to_string()),
                },
            ),
            (
                "baz".to_string(),
                Tag {
                    key: "baz".to_string(),
                    value: Some("".to_string()),
                },
            ),
            (
                "quux".to_string(),
                Tag {
                    key: "quux".to_string(),
                    value: None,
                },
            ),
        ]
        .into_iter()
        .collect();

        assert_eq!(tags, Some(Tags(expected)));
    }
}
