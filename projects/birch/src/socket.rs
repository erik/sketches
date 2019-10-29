use std::io::prelude::*;
use std::io::{BufRead, Error, ErrorKind, Result};

use crossbeam_channel::{Receiver, Sender};
use mio::net::TcpStream;

use crate::proto::RawMessage;

pub trait IrcWriter {
    fn write_message(&mut self, msg: &RawMessage) -> Result<()>;
}

pub trait IrcReader {
    fn read_message(&mut self) -> Result<RawMessage>;
}

impl<T: Write> IrcWriter for T {
    fn write_message(&mut self, msg: &RawMessage) -> Result<()> {
        let msg_str = msg.to_string();
        self.write_all(msg_str.as_bytes())?;
        self.write_all(b"\r\n")?;

        Ok(())
    }
}

impl<T: BufRead> IrcReader for T {
    fn read_message(&mut self) -> Result<RawMessage> {
        let mut buf = String::new();
        self.read_line(&mut buf)?;

        let line = buf.trim_end_matches(|c| c == '\r' || c == '\n');
        RawMessage::parse(line).ok_or_else(|| {
            Error::new(ErrorKind::Other, "failed to parse message from line")
        })
    }
}

#[derive(Clone)]
pub struct IrcSocketConfig {
    // TODO: stop being lazy, make this &str
    pub addr: String,
    pub max_retries: Option<usize>,
}

pub struct IrcSocket {
    config: IrcSocketConfig,
    to_network: (Sender<RawMessage>, Receiver<RawMessage>),
}

impl IrcSocket {
    /// establish a connection to the specied address.
    ///
    /// Eventually this is will include a sleep / exponential backoff
    /// (TODO: that)
    fn try_create_stream(&self) -> Result<TcpStream> {
        let max_retries = self.config.max_retries;

        let mut i = 0;
        loop {
            let stream = std::net::TcpStream::connect(&self.config.addr);
            let over_max_tries = max_retries.map(|max| i > max).unwrap_or(false);

            match stream {
                Ok(stream) => return TcpStream::from_stream(stream),
                Err(err) => {
                    if over_max_tries {
                        return Err(err);
                    }
                }
            }

            i += 1
        }
    }
}
