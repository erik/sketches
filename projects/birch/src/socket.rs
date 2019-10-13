use std::io::prelude::*;
use std::io::{BufRead, BufReader, Error, ErrorKind, Result};
use std::net::TcpStream;
use std::time::Duration;

use crate::network::NetworkConnection;
use crate::proto::RawMessage;

pub trait IrcWriter {
    fn write_message(&mut self, msg: &RawMessage) -> Result<()> {
        let msg_str = msg.to_string();
        println!("[\u{1b}[37;1mbirch\u{1b}[0m -> net] {}", msg_str);
        self.write_raw(&msg_str)
    }

    fn write_raw(&mut self, msg: &str) -> Result<()>;
}

pub trait IrcReader {
    fn read_message(&mut self) -> Result<RawMessage>;
}

impl<T: Write> IrcWriter for T {
    fn write_raw(&mut self, msg: &str) -> Result<()> {
        self.write_all(msg.as_bytes())?;
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

pub struct IrcSocketConfig<'a> {
    pub nick: &'a str,
    pub addr: &'a str,
    pub max_retries: Option<usize>,
}

pub struct IrcSocket<'a> {
    config: IrcSocketConfig<'a>,
}

impl<'a> IrcSocket<'a> {
    pub fn new(config: IrcSocketConfig<'a>) -> Self {
        Self { config }
    }

    // TODO: this is kind of gross, clean this up
    fn create_stream(&self) -> Result<TcpStream> {
        let _create_stream = || {
            let stream = TcpStream::connect(self.config.addr)?;
            stream.set_read_timeout(Some(Duration::from_secs(180)))?;
            Ok(stream)
        };

        let max_retries = self.config.max_retries;

        let mut i = 0;
        loop {
            let stream = _create_stream();
            let over_max_tries = max_retries.map(|max| i > max).unwrap_or(false);

            if stream.is_ok() || over_max_tries {
                return stream;
            }

            i += 1
        }
    }

    /// On connection close, return `true` when the connection should
    /// be restarted, and false otherwise. If there is a
    /// non-recoverable exception, return the error.
    ///
    /// TODO: Need to come up with the clean exit concept.
    fn connect(&self, users: &mut dyn IrcWriter) -> Result<bool> {
        let mut stream = self.create_stream()?;
        let mut reader = BufReader::new(stream.try_clone()?);
        let mut network = NetworkConnection::new(self.config.nick, &mut stream, users);

        if network.initialize().is_err() {
            return Ok(true);
        }

        loop {
            let result = reader.read_message().and_then(|msg| {
                println!("[birch <- \u{1b}[37;1mnet\u{1b}[0m] {}", msg);
                network.handle(&msg)
            });

            if let Err(err) = result {
                println!("read failed: {}", err);
                break;
            }
        }
        Ok(true)
    }

    pub fn start_loop(&mut self, users: &mut dyn IrcWriter) -> Result<()> {
        loop {
            match self.connect(users) {
                Ok(true) => println!("connection terminated, restarting"),
                Ok(false) => break,
                Err(err) => {
                    println!("connection failed: {:?}", err);
                    return Err(err);
                }
            }
        }
        Ok(())
    }
}
