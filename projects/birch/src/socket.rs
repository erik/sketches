use std::io::prelude::*;
use std::io::{BufRead, Error, ErrorKind, Result};

use crate::proto::RawMessage;

pub trait IRCWriter {
    fn write_message(&mut self, msg: &RawMessage) -> Result<()> {
        let msg_str = msg.to_string();
        println!("[\u{1b}[37;1mbirch\u{1b}[0m -> net] {}", msg_str);
        self.write_raw(&msg_str)
    }

    fn write_raw(&mut self, msg: &str) -> Result<()>;
}

pub trait IRCReader {
    fn read_message(&mut self) -> Result<RawMessage>;
}

impl<T: Write> IRCWriter for T {
    fn write_raw(&mut self, msg: &str) -> Result<()> {
        self.write_all(msg.as_bytes())?;
        self.write_all(b"\r\n")?;

        Ok(())
    }
}

impl<T: BufRead> IRCReader for T {
    fn read_message(&mut self) -> Result<RawMessage> {
        let mut buf = String::new();
        self.read_line(&mut buf)?;

        let line = buf.trim_end_matches(|c| c == '\r' || c == '\n');
        RawMessage::parse(line).ok_or_else(|| {
            Error::new(ErrorKind::Other, "failed to parse message from line")
        })
    }
}

struct IRCSocket {}
