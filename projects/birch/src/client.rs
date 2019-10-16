#![allow(dead_code, unused_variables)]

use std::collections::HashSet;
use std::io::{Error, ErrorKind, Result};
use std::time::Instant;

use crossbeam_channel::Sender;

use crate::proto::{Capability, MessageKind, RawMessage, Source};

enum AuthState {
    NeedsAuth,
    Authenticated,
}

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
    Authenticated,
}

/// Represents User <-> Birch connection
// TODO: Rename. Maybe this is more about client state?
pub struct ClientConnection {
    auth: AuthState,
    caps: HashSet<Capability>,
    last_ping_pong: (Option<Instant>, Option<Instant>),

    events: Sender<ClientEvent>,
}

impl ClientConnection {
    pub fn new(events: Sender<ClientEvent>) -> Self {
        Self {
            auth: AuthState::NeedsAuth,
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

    fn send_client_error(&mut self, msg: &str) -> Result<()> {
        self.send_client("ERROR", &[msg])
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

            _ => {
                // TODO: Send invalid command
            }
        }
        Ok(())
    }

    fn handle_unauthenticated(&mut self, msg: &RawMessage) -> Result<bool> {
        let authed = match MessageKind::from(msg) {
            MessageKind::Pass => {
                // TODO: actually handle auth here
                if let Some(auth) = msg.param(0).and_then(ClientAuth::parse) {
                    self.auth = AuthState::Authenticated;
                    true
                } else {
                    self.send_client_error("you must authenticate first")?;
                    false
                }
            }
            MessageKind::User => {
                // TODO: handle user
                false
            }
            MessageKind::Nick => {
                // TODO: handle user
                false
            }
            MessageKind::Capability => {
                self.handle_cap_command(msg)?;
                false
            }
            _ => {
                self.send_client_error("you must authenticate first")?;
                false
            }
        };

        if authed {
            self.send_client_event(ClientEvent::Authenticated)?;
        }

        // Never want to forward unauthenticated messages on to the
        // server.
        Ok(false)
    }

    fn handle_authenticated(&mut self, msg: &RawMessage) -> Result<bool> {
        Ok(match MessageKind::from(msg) {
            MessageKind::Ping => {
                let param = msg.param(0).unwrap_or("");
                self.send_client("PONG", &[param])?;
                // No need to forward PING messages, we respond ourselves.
                false
            }

            MessageKind::Pong => {
                self.last_ping_pong.1 = Some(Instant::now());
                false
            }

            MessageKind::Capability => {
                self.handle_cap_command(msg)?;
                false
            }

            MessageKind::User => false,

            MessageKind::Nick => {
                // self.send_client_event(ClientEvent::ChangeNick)
                false
            }

            kind => {
                println!("Unhandled client message kind: {:?}", kind);
                false
            }
        })
    }

    /// Relies on assumption that this function won't be called more
    /// frequently than the ping interval.
    pub fn ping(&mut self) -> Result<()> {
        match self.last_ping_pong {
            // If we haven't received a PONG (or it's been more than 120 since receiving one, fail connection.)
            (Some(_ping), pong)
                if pong.map(|it| it.elapsed().as_secs() > 120).unwrap_or(true) =>
            {
                self.send_client_error("ping time out")
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

        let should_forward = match self.auth {
            AuthState::NeedsAuth => self.handle_unauthenticated(msg),
            AuthState::Authenticated => self.handle_authenticated(msg),
        };

        match should_forward {
            Ok(true) => self.send_client_event(ClientEvent::WriteNetwork(msg.clone())),
            Ok(false) => Ok(()),
            Err(err) => {
                println!("failure while handling client message: {}", err);
                Err(err)
            }
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
