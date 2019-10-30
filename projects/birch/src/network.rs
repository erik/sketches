use std::collections::HashSet;
use std::io::Result;

use crate::proto::{Capability, MessageKind, ModeSet, RawMessage, Source};
use crate::socket::IrcWriter;

enum AuthKind {
    SaslPlain(String, String),
    Pass(String),
    None,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum ConnectionStatus {
    Initial,
    CapabilityNegotiation,
    Authentication,
    Registration,
    Registered,
}

/// Ephemeral state of a socket connection to an IRC network
pub struct NetworkConnectionState {
    nick: String,
    user_modes: ModeSet,
    caps_pending: usize,
    caps: HashSet<Capability>,
    status: ConnectionStatus,

    // These values may vary based on the server connected to even
    // within the same network, so we don't want to persist these long
    // term.
    init_buffer: Vec<RawMessage>,
    motd_buffer: Vec<RawMessage>,
}

impl NetworkConnectionState {
    fn new(nick: &str) -> Self {
        Self {
            nick: nick.to_string(),
            user_modes: ModeSet::empty(),
            caps_pending: 0,
            caps: HashSet::new(),
            status: ConnectionStatus::Initial,
            init_buffer: Vec::with_capacity(10),
            motd_buffer: Vec::with_capacity(50),
        }
    }

    /// Reset the connection state back to the initial, keeping only the nick.
    fn reset(&mut self) {
        *self = Self::new(&self.nick);
    }

    // TODO: Enforce transition validity?
    fn transition(&mut self, next: ConnectionStatus) {
        self.status = next;
    }

    fn next_nick(&self) -> String {
        // TODO: Check that nick len < 16?
        self.nick.clone() + "`"
    }

    fn add_init_msg(&mut self, msg: &RawMessage) {
        self.init_buffer.push(msg.clone());
    }

    fn add_motd_msg(&mut self, msg: &RawMessage) {
        self.motd_buffer.push(msg.clone());
    }

    fn clear_motd(&mut self) {
        self.motd_buffer.clear();
    }

    pub fn welcome_user(&self, w: &mut impl IrcWriter) -> Result<()> {
        let birch = Source {
            nick: "birch".to_string(),
            ident: None,
            host: None,
            is_server: true,
        };

        w.write_message(&RawMessage::new_with_source(
            birch.clone(),
            "001",
            &[&self.nick, "welcome to birch"],
        ))?;

        for msg in self.init_buffer.iter() {
            let mut msg = msg.clone();
            msg.source = Some(birch.clone());
            w.write_message(&msg)?;
        }

        for msg in self.motd_buffer.iter() {
            let mut msg = msg.clone();
            msg.source = Some(birch.clone());
            w.write_message(&msg)?;
        }

        Ok(())
    }
}

/// Represents Birch <-> IRC network connection
// TODO: Perhaps this is a network state?
pub struct NetworkConnection {
    auth: AuthKind,
    pub state: NetworkConnectionState,

    network: Vec<RawMessage>,
    users: Vec<RawMessage>,
}

impl NetworkConnection {
    pub fn new(nick: &str) -> Self {
        Self {
            auth: AuthKind::None,
            state: NetworkConnectionState::new(nick),

            network: Vec::with_capacity(32),
            users: Vec::with_capacity(32),
        }
    }

    pub fn initialize(&mut self) -> Result<()> {
        self.state.reset();
        self.transition_status(ConnectionStatus::Initial)
    }

    pub fn user_messages(&mut self) -> std::vec::Drain<RawMessage> {
        self.users.drain(0..)
    }

    pub fn network_messages(&mut self) -> std::vec::Drain<RawMessage> {
        self.network.drain(0..)
    }

    fn send_network(&mut self, command: &str, params: &[&str]) {
        self.network.push(RawMessage::new(command, params));
    }

    fn send_users(&mut self, msg: RawMessage) {
        self.users.push(msg);
    }

    fn transition_status(&mut self, next: ConnectionStatus) -> Result<()> {
        match &next {
            ConnectionStatus::Initial => {
                self.transition_status(ConnectionStatus::CapabilityNegotiation)?;
            }

            ConnectionStatus::CapabilityNegotiation => {
                self.send_network("CAP", &["LS", "302"]);
            }

            ConnectionStatus::Authentication => match self.auth {
                AuthKind::SaslPlain(ref _user, ref _pass) => unimplemented!(),
                AuthKind::Pass(ref _pass) => unimplemented!(),
                AuthKind::None => {
                    self.transition_status(ConnectionStatus::Registration)?;
                }
            },

            ConnectionStatus::Registration => {
                // TODO: prevent unnecessary clone
                let nick = self.state.nick.clone();

                self.send_network("NICK", &[&nick]);
                self.send_network("USER", &[&nick, "0", "*", &nick]);

                self.transition_status(ConnectionStatus::Registered)?;
            }

            ConnectionStatus::Registered => (),
        }

        self.state.transition(next);
        Ok(())
    }

    fn handle_cap_message(&mut self, msg: &RawMessage) -> Result<()> {
        let cmd = msg.param(1).unwrap_or("");
        match cmd {
            "LS" => {
                let caps = msg.trailing().unwrap_or("").split_whitespace();

                for cap_str in caps {
                    if Capability::from(cap_str).is_some() {
                        self.send_network("CAP", &["REQ", cap_str]);
                        self.state.caps_pending += 1;
                    }
                }
            }

            "ACK" | "NAK" => {
                let cap = msg.trailing().and_then(Capability::from);

                if let Some(cap) = cap {
                    if cmd == "ACK" {
                        println!("CAP {:?} enabled", cap);
                        self.state.caps.insert(cap);
                    }
                }

                self.state.caps_pending -= 1;

                if self.state.caps_pending == 0 {
                    self.send_network("CAP", &["END"]);
                    self.transition_status(ConnectionStatus::Authentication)?;
                }
            }

            // Not strictly necessary but if we want to support `cap-notify`
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
                self.send_network("PONG", &[param]);
                false
            }

            MessageKind::Capability => {
                self.handle_cap_message(msg)?;
                // We have our own caps, don't send the server's.
                false
            }

            MessageKind::Numeric(code) => self.handle_numeric(code, msg)?,

            MessageKind::Privmsg | MessageKind::Notice => {
                if let Some(ref source) = msg.source {
                    let sender = &source.nick;
                    let msg = msg.trailing().unwrap_or("");

                    // Handle basic CTCP
                    match msg {
                        "\x01VERSION\x01" => {
                            // TODO: Probably should be configurable
                            let version = "\x01VERSION birch\x01";
                            self.send_network("NOTICE", &[sender, version]);
                            false
                        }
                        _ => true,
                    }
                } else {
                    // Invalid msg? No sender.
                    false
                }
            }

            // MessageKind::Join => true,
            // MessageKind::Part => true,
            // MessageKind::Quit => true,
            // MessageKind::Kick => true,
            MessageKind::Mode => {
                if let Some(target) = msg.param(0) {
                    if target == self.state.nick {
                        let modes =
                            msg.param(1).and_then(|s| self.state.user_modes.apply(s));
                        if let Some(modes) = modes {
                            self.state.user_modes = modes;
                        }
                    } else {
                        // TODO: channel modes, other users
                    }
                }
                true
            }

            _ => {
                println!("Unhandled message: {:?}", msg);
                true
            }
        };

        if should_forward {
            // TODO: remove clone
            self.send_users(msg.clone());
        }

        Ok(())
    }

    fn handle_numeric(&mut self, code: u16, msg: &RawMessage) -> Result<bool> {
        let should_forward = match code {
            1 => {   // :server 001 nick :welcome message
                // Don't need to forward this since we have our own
                false
            }

            2 |      // Your host
            3 |      // Server created at
            4 |      // Server info
            5 |      // ISUPPORT (TODO: parse this)
            250 |    // Highest user count
            251 |    // User count
            252 |    // Oper count
            253 |    // Unknown connections
            254 |    // Channel count
            255 |    // Client count
            265 |    // Local users
            266 => { // Global users
                self.state.add_init_msg(msg);
                true
            },

            305 => { // Clear AWAY
                true
            }

            306 => { // Set AWAY
                true
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
                self.state.clear_motd();
                true
            }

            372 |    // MOTD
            376 => { // End MOTD
                self.state.add_motd_msg(msg);
                true
            }

            432 |     // :irc.server.com 432 * nick :Erroneous Nickname
            433 => {  // :irc.server.com 433 * nick :Nickname is already in use
                // TODO: Handle 432 separately? Nick too long or bad characters
                let alt = self.state.next_nick();

                self.send_network("NICK", &[&alt]);
                // Tell the connected users that we're renaming
                self.send_users(RawMessage::new_with_source(
                    Source {
                        nick: self.state.nick.clone(),
                        ident: None,
                        host: None,
                        is_server: true
                    },
                    "NICK",
                    &[&alt]
                ));

                self.state.nick = alt;
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

            // Other numerics could be extensions that we're unaware
            // of, better to forward it just in case.
            _ => {
                println!("unhandled numeric: {}", code);
                true
            }
        };

        Ok(should_forward)
    }
}
