pub mod proto;

use proto::RawMessage;

// TODO: Split this out into somewhere else
trait IRCWriter {
    fn write_message(&mut self, msg: &RawMessage) -> std::io::Result<()> {
        let msg_str = msg.to_string();
        self.write_raw(&msg_str)
    }

    fn write_raw(&mut self, msg: &str) -> std::io::Result<()>;
}

// TODO: Move this elsewhere.
mod sock {
    use std::io::prelude::*;
    use std::net::TcpStream;

    impl crate::IRCWriter for TcpStream {
        fn write_raw(&mut self, msg: &str) -> std::io::Result<()> {
            self.write_all(msg.as_bytes())?;
            Ok(())
        }
    }
}

pub mod client;
pub mod cursor;
pub mod server;
