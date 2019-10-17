#![allow(dead_code, unused_variables)]

use std::collections::HashSet;
use std::io::{Error, ErrorKind, Result};
use std::time::Instant;

use crossbeam_channel::Sender;

use crate::proto::{Capability, MessageKind, RawMessage, Source};

/// Represented in `PASS` commands as 'user@client_id/network:password'.
/// For now, each part will be mandatory.
#[derive(Debug, PartialEq)]
struct ClientAuth {
    user: String,
    client_id: String,
    network: String,
    password: String,
}

impl ClientAuth {
    fn parse(s: &str) -> Option<ClientAuth> {
        let colon = s.find(':')?;
        let slash = s[..colon].rfind('/')?;
        let at = s[..slash].rfind('@')?;

        Some(ClientAuth {
            user: s[..at].to_string(),
            client_id: s[at + 1..slash].to_string(),
            network: s[slash + 1..colon].to_string(),
            password: s[colon + 1..].to_string(),
        })
    }
}

pub enum ClientEvent {
    WriteNetwork(RawMessage),
    WriteClient(RawMessage),
    RegistrationComplete,
}

/// Keeps track of the various commands required to complete registration.
struct RegistrationState {
    pass: bool,
    nick: bool,
    user: bool,
}

impl RegistrationState {
    fn new() -> Self {
        Self {
            pass: false,
            nick: false,
            user: false,
        }
    }

    fn set_pass(&mut self) {
        self.pass = true;
    }

    fn set_user(&mut self) {
        self.user = true;
    }

    fn set_nick(&mut self) {
        self.nick = true;
    }

    fn is_complete(&self) -> bool {
        self.pass && self.user && self.nick
    }
}

/// Represents User <-> Birch connection
// TODO: Rename. Maybe this is more about client state?
pub struct ClientConnection {
    nick: String,
    registration: RegistrationState,
    caps: HashSet<Capability>,
    last_ping_pong: (Option<Instant>, Option<Instant>),

    events: Sender<ClientEvent>,
}

impl ClientConnection {
    pub fn new(events: Sender<ClientEvent>) -> Self {
        Self {
            nick: "*".to_string(),
            registration: RegistrationState::new(),
            caps: HashSet::new(),
            last_ping_pong: (None, None),

            events,
        }
    }

    fn map_send_error<E>(_: E) -> Error {
        Error::new(ErrorKind::Other, "send event failed")
    }

    fn send_client_event(&mut self, event: ClientEvent) -> Result<()> {
        self.events
            .send(event)
            .map_err(ClientConnection::map_send_error)
    }

    fn send_network_message(&mut self, msg: RawMessage) -> Result<()> {
        self.send_client_event(ClientEvent::WriteNetwork(msg))
    }

    fn send_message(&mut self, msg: RawMessage) -> Result<()> {
        self.send_client_event(ClientEvent::WriteClient(msg))
    }

    // TODO: better name
    fn send_client(&mut self, command: &str, params: &[&str]) -> Result<()> {
        self.send_message(RawMessage::new_with_source(
            Source::birch(),
            command,
            params,
        ))
    }

    fn send_numeric_reply(&mut self, code: &str, msg: &str) -> Result<()> {
        let nick = &self.nick.clone();
        self.send_client(code, &[nick, msg])
    }

    fn handle_cap_command(&mut self, msg: &RawMessage) -> Result<()> {
        match msg.param(0).unwrap_or("") {
            "LS" => unimplemented!(),
            "END" => unimplemented!(),
            "REQ" => {
                for cap_str in msg.param(1).unwrap_or("").split_whitespace() {
                    if let Some(cap) = Capability::from(cap_str) {
                        self.send_client("CAP", &["ACK", cap_str])?;
                        self.caps.insert(cap);
                    } else {
                        self.send_client("CAP", &["NAK", cap_str])?;
                    }
                }
            }
            // TODO: 401 <nick> <command> :Invalid CAP command
            _ => self.send_numeric_reply("410", "Invalid CAP command")?,
        }
        Ok(())
    }

    fn handle_unregistered(&mut self, msg: &RawMessage) -> Result<()> {
        match MessageKind::from(msg) {
            MessageKind::Pass => {
                // TODO: actually handle auth here
                // TODO: We don't know WHICH network the client wants
                // until they actually authenticate.
                if let Some(auth) = msg.param(0).and_then(ClientAuth::parse) {
                    self.registration.set_pass();
                } else {
                    self.send_numeric_reply("464", "Password incorrect")?;
                }
            }
            // TODO: Do we care about this USER command, or just that it happens?
            MessageKind::User => self.registration.set_user(),
            MessageKind::Nick => match msg.param(0) {
                Some(nick) => {
                    self.nick = nick.to_string();
                    self.registration.set_nick();
                }
                None => self.send_numeric_reply("461", "Not enough parameters")?,
            },
            MessageKind::Capability => self.handle_cap_command(msg)?,
            _ => self.send_numeric_reply("451", "You are not registered")?,
        };

        if self.registration.is_complete() {
            self.send_client_event(ClientEvent::RegistrationComplete)?;
        }

        Ok(())
    }

    fn handle_registered(&mut self, msg: &RawMessage) -> Result<()> {
        match MessageKind::from(msg) {
            MessageKind::Pass | MessageKind::User => {
                self.send_numeric_reply("462", "You may not reregister")?;
            }

            // No need to forward PING messages, we respond ourselves.
            MessageKind::Ping => {
                let param = msg.param(0).unwrap_or("");
                self.send_client("PONG", &[param])?;
            }

            MessageKind::Pong => {
                self.last_ping_pong.1 = Some(Instant::now());
            }

            MessageKind::Capability => {
                self.handle_cap_command(msg)?;
            }

            MessageKind::Join => {}
            MessageKind::Part => {}

            // TODO: Coordinate nick changes with server. We also need
            // to be informed BY the server of changes from other
            // connected clients.
            MessageKind::Nick => {}

            kind => {
                println!("Unhandled client message kind: {:?}", kind);
            }
        }

        Ok(())
    }

    /// Relies on assumption that this function won't be called more
    /// frequently than the ping interval.
    pub fn ping(&mut self) -> Result<()> {
        match self.last_ping_pong {
            // If we haven't received a PONG (or it's been more than 120 since receiving one, fail connection.)
            (Some(_ping), pong)
                if pong.map(|it| it.elapsed().as_secs() > 120).unwrap_or(true) =>
            {
                self.send_client("ERROR", &["ping time out"])
                    .and_then(|_| Err(Error::new(ErrorKind::Other, "ping time out")))
            }

            // Either haven't yet sent a PING or the client has responded within last 120 seconds
            _ => {
                self.last_ping_pong.0 = Some(Instant::now());
                self.send_client("PING", &[])
            }
        }
    }

    pub fn handle_message(&mut self, msg: &RawMessage) -> Result<()> {
        println!("From client: {:?}", msg);

        if !self.registration.is_complete() {
            self.handle_unregistered(msg)
        } else {
            self.handle_registered(msg)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_client_auth() {
        let auth = ClientAuth::parse("user@client_id/network:password");
        assert_eq!(
            auth,
            Some(ClientAuth {
                user: "user".to_string(),
                client_id: "client_id".to_string(),
                network: "network".to_string(),
                password: "password".to_string()
            })
        )
    }
}
