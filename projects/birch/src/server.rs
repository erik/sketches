#![allow(dead_code, unused_variables)]

use std::collections::HashSet;

use crate::proto::{Capability, MessageKind, RawMessage};

/// Represents Birch <-> IRC network connection
// TODO: Rename NetworkConnection?
struct ServerConnection {
    nick: String,
    caps: HashSet<Capability>,
    // writer: dyn IRCWriter,
    // user_fanout: dyn IRCWriter,
}

impl ServerConnection {
    fn handle(&mut self, msg: &RawMessage) {
        let kind = MessageKind::from(msg);

        let should_forward = match kind {
            MessageKind::Ping => {
                // TODO: Respond to network PING
                false
            }

            _ => true,
        };

        if should_forward {
            // TODO: send message to user_fanout
        }
    }
}
