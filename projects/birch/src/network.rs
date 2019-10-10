#![allow(dead_code, unused_variables)]

use std::collections::HashSet;
use std::io::Result;

use crate::proto::{Capability, MessageKind, RawMessage};
use crate::socket::IRCWriter;

enum AuthKind {
    SaslPlain(String, String),
    Pass(String),
    None,
}

#[derive(Debug, Copy, Clone)]
enum ConnectionState {
    Initial,
    CapabilityNegotiation,
    Authentication,
    Registration,
    Registered,
}

/// Represents Birch <-> IRC network connection
pub struct NetworkConnection<'a> {
    nick: String,
    caps_pending: usize,
    caps: HashSet<Capability>,
    auth: AuthKind,

    state: ConnectionState,

    writer: &'a mut dyn IRCWriter,
    // user_fanout: &'a mut dyn IRCWriter,
}

impl<'a> NetworkConnection<'a> {
    pub fn new(nick: &str, writer: &'a mut dyn IRCWriter) -> Self {
        Self {
            nick: nick.to_string(),
            caps_pending: 0,
            caps: HashSet::new(),
            auth: AuthKind::None,

            state: ConnectionState::Initial,

            writer,
        }
    }

    pub fn initialize(&mut self) -> Result<()> {
        self.transition_state(ConnectionState::Initial)
    }

    fn register(&mut self) -> Result<()> {
        let msgs = vec![
            RawMessage::new("NICK", &[&self.nick]),
            RawMessage::new("USER", &[&self.nick, "0", "*", &self.nick]),
        ];

        for m in msgs.iter() {
            self.writer.write_message(&m)?;
        }

        Ok(())
    }

    fn write_network(&mut self, command: &str, params: &[&str]) -> Result<()> {
        self.writer.write_message(&RawMessage::new(command, params))
    }

    fn transition_state(&mut self, next: ConnectionState) -> Result<()> {
        match &next {
            ConnectionState::Initial => {
                self.transition_state(ConnectionState::CapabilityNegotiation)?;
            }

            ConnectionState::CapabilityNegotiation => {
                self.write_network("CAP", &["LS", "302"])?;
            }

            ConnectionState::Authentication => match self.auth {
                AuthKind::SaslPlain(ref _user, ref _pass) => unimplemented!(),
                AuthKind::Pass(ref _pass) => unimplemented!(),
                AuthKind::None => {
                    self.transition_state(ConnectionState::Registration)?;
                }
            },

            ConnectionState::Registration => {
                // TODO: prevent unnecessary clone
                let nick = self.nick.clone();

                self.write_network("NICK", &[&nick])?;
                self.write_network("USER", &[&nick, "0", "*", &nick])?;

                self.transition_state(ConnectionState::Registered)?;
            }

            ConnectionState::Registered => (),
        }

        Ok(())
    }

    fn handle_cap_message(&mut self, msg: &RawMessage) -> Result<()> {
        let cmd = msg.param(1).unwrap_or("");
        match cmd {
            "LS" => {
                let caps = msg.trailing().unwrap_or("").split_whitespace();

                for cap_str in caps {
                    if let Some(cap) = Capability::from(cap_str) {
                        self.write_network("CAP", &["REQ", cap_str])?;
                        self.caps_pending += 1;
                    }
                }
            }

            "ACK" | "NAK" => {
                let cap = msg.trailing().and_then(Capability::from);

                if let Some(cap) = cap {
                    if cmd == "ACK" {
                        println!("CAP {:?} enabled", cap);
                        self.caps.insert(cap);
                    }
                }

                self.caps_pending -= 1;

                if self.caps_pending == 0 {
                    self.write_network("CAP", &["END"])?;
                    self.transition_state(ConnectionState::Authentication)?;
                }
            }

            "NEW" | "DEL" => unimplemented!(),

            _ => println!("ignoring unknown cap command: {}", msg),
        }

        Ok(())
    }

    pub fn handle(&mut self, msg: &RawMessage) -> Result<()> {
        let kind = MessageKind::from(msg);
        let should_forward = match kind {
            MessageKind::Ping => {
                let param = msg.param(0).unwrap_or("");
                self.write_network("PONG", &[param])?;
                false
            }

            MessageKind::Capability => {
                self.handle_cap_message(msg)?;

                // We have our own caps, don't send the servers.
                false
            }

            MessageKind::Numeric(code) => self.handle_numeric(code, msg)?,

            // MessageKind::Privmsg => true,
            // MessageKind::Notice => true,

            // MessageKind::Join => true,
            // MessageKind::Part => true,
            // MessageKind::Quit => true,

            // MessageKind::Kick => true,

            // MessageKind::Mode => true,
            _ => {
                println!("Unhandled message: {:?}", msg);

                true
            }
        };

        if should_forward {
            // TODO: send message to user_fanout
        }

        Ok(())
    }

    fn handle_numeric(&mut self, code: u16, msg: &RawMessage) -> Result<bool> {
        let should_forward = match code {
            // :server 001 nick :welcome message
            1 => false,

            // ISUPPORT
            5 => false,

            2 |      // Your host
            3 |      // Server created at
            4 |      // Server info
            250 |    // Highest user count
            251 |    // User count
            252 |    // Oper count
            253 |    // Unknown connections
            254 |    // Channel count
            255 |    // Client count
            265 |    // Local users
            266 => { // Global users
                // TODO: Update network buffer state
                false
            },

            305 => { // Clear AWAY
                false
            }

            306 => { // Set AWAY
                false
            }

            329 => { // :server.com 329 nick #chan <epoch chan creation>
                false
            }

            // :server.com 331 nick #chan :no topic is set
            331 => {
                false
            }

            // :server.com 332 nick #chan :channel topic
            332 => {
                false
            }

            // :server.com 333 nick #chan who_set <epochtime>
            333 => {
                false
            }

            353 |    // :server.com 353 nick @ #chan :nick1 nick2
            366 => { // :server.com 366 nick #chan :End of /NAMES
                false
            }

            375 |    // Begin MOTD
            422 => { // MOTD file is missing
                // TODO: Clear MOTD
                false
            }

            372 |    // MOTD
            376 => { // End MOTD
                // TODO: MOTD
                false
            }

            432 |
            433 => { // :irc.server.com 432 * nick :Erroneous Nickname
                false
            }

            // Multiple possible:
            //
            // :irc.server.net 437 * badnick :Nick is temporarily unavailable
            // :irc.server.net 437 nick badnick :Nick is temporarily unavailable
            // :irc.server.net 437 nick badnick :Cannot change nick while banned on channel
            //
            // Only want to try an alt nick in the first case.
            437 => {
                false
            }

            _ => {
                println!("unhandled numeric: {}", code);

                true
            }
        };

        Ok(should_forward)
    }
}
