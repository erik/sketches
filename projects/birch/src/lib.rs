pub mod proto;

use proto::RawMessage;

// TODO: Split this out into somewhere else
trait IRCWriter {
    fn write(msg: &RawMessage) -> std::io::Result<()>;
    fn write_raw(msg: &str) -> std::io::Result<()>;
}

pub mod client;
pub mod cursor;
pub mod server;
