#![allow(dead_code, unused_variables)]

use std::collections::HashSet;
use std::io::Result;

use crate::proto::{Capability, MessageKind, RawMessage, Source};
use crate::socket::IrcWriter;

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

/// Represents User <-> Birch connection
// TODO: Rename UserConnection?
pub struct ClientConnection<'a> {
    auth: AuthState,
    caps: HashSet<Capability>,

    writer: &'a mut dyn IrcWriter,
}

impl<'a> ClientConnection<'a> {
    pub fn new(writer: &'a mut impl IrcWriter) -> Self {
        Self {
            auth: AuthState::NeedsAuth,
            caps: HashSet::new(),

            writer,
        }
    }

    fn send_message(&mut self, msg: &RawMessage) -> Result<()> {
        self.writer.write_message(msg)
    }

    // TODO: better name
    fn send_client(&mut self, command: &str, params: &[&str]) -> Result<()> {
        self.writer.write_message(&RawMessage::new_with_source(
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
        match MessageKind::from(msg) {
            MessageKind::Pass => {
                // TODO: actually handle auth here
                if let Some(auth) = msg.param(0).and_then(ClientAuth::parse) {
                    self.auth = AuthState::Authenticated;
                } else {
                    self.send_client_error("you must authenticate first")?;
                }
            }
            MessageKind::User => {
                // TODO: handle user
            }
            MessageKind::Nick => {
                // TODO: handle user
            }
            MessageKind::Capability => self.handle_cap_command(msg)?,
            _ => {
                self.send_client_error("you must authenticate first")?;
            }
        };

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

            _ => true,
        })
    }

    pub fn handle_message(&mut self, msg: &RawMessage) -> Result<()> {
        println!("From client: {:?}", msg);

        let should_forward = match self.auth {
            AuthState::NeedsAuth => self.handle_unauthenticated(msg),
            AuthState::Authenticated => self.handle_authenticated(msg),
        };

        match should_forward {
            Ok(true) => Ok(()), // TODO: write to network
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
