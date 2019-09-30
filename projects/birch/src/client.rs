#![allow(dead_code, unused_variables)]

use std::collections::HashSet;

use crate::proto::{MessageKind, RawMessage};

trait ClientWriter {
    fn write(msg: &RawMessage) -> std::io::Result<()>;
    fn write_raw(msg: &str) -> std::io::Result<()>;
}

enum AuthState {
    NeedsAuth,
    Authenticated,
}

/// Represented in `PASS` commands as 'user@client_id/network:password'.
/// For now, each part will be mandatory.
struct ClientAuth {
    user: String,
    client_id: String,
    network: String,
    password: String
}

impl ClientAuth {
    fn parse(s: &str) -> Option<ClientAuth> {
        let colon = s.find(':')?;
        let slash = s[..colon].rfind('/')?;
        let at = s[..slash].rfind('@')?;

        Some(ClientAuth {
            user: s[..at].to_string(),
            client_id: s[at+1..slash].to_string(),
            network: s[slash+1..colon].to_string(),
            password: s[colon+1..].to_string(),
        })
    }
}

#[derive(Hash, PartialEq, Eq)]
enum ClientCapability {
    ServerTime,
}

impl ClientCapability {
    fn from(s: &str) -> Option<ClientCapability> {
        match s {
            "server-time" => Some(Self::ServerTime),
            _ => None,
        }
    }
}

/// Represents User <-> Birch connection
struct Client {
    auth: AuthState,
    caps: HashSet<ClientCapability>,
    // writer: dyn ClientWriter,
}

impl Client {
    fn new() -> Self {
        Client {
            auth: AuthState::NeedsAuth,
            caps: HashSet::new(),
        }
    }

    fn handle_cap_command(&mut self, msg: &RawMessage) {
        match msg.param(0).unwrap_or("missing") {
            "LS" => unimplemented!(),
            "END" => unimplemented!(),
            "REQ" => {
                msg.param(1).unwrap_or("")
                    .split_whitespace()
                    .map(ClientCapability::from)
                    .for_each(|cap| match cap {
                        Some(cap) => {
                            self.caps.insert(cap);
                            // TODO: respond with ACK
                        },
                        None => {
                            // TODO: respond with NAK
                        }
                    });
            },
            _ => {
                // TODO: Send invalid command
            }
        }
    }

    fn handle_unauthenticated(&mut self, msg: &RawMessage) -> bool {
        match MessageKind::from(msg) {
            MessageKind::Pass => {
                let auth = msg.param(0).map(ClientAuth::parse);

                // TODO: handle auth here
            }
            MessageKind::User => {
                // TODO: handle user
            }
            MessageKind::Nick => {
                // TODO: handle user
            }
            MessageKind::Capability => self.handle_cap_command(msg),
            _ => {
                // TODO: Log error not authenticated here
            }

        };

        // Never want to forward unauthenticated messages on to the
        // server.
        false
    }

    fn handle_authenticated(&mut self, msg: &RawMessage) -> bool {
        match MessageKind::from(msg) {
            _ => true
        }
    }

    fn handle(&mut self, msg: &RawMessage) {
        let should_forward = match self.auth {
            AuthState::NeedsAuth => self.handle_unauthenticated(msg),
            AuthState::Authenticated => self.handle_authenticated(msg),
        };

        if should_forward {
            // TODO: write to server
        }
    }
}
