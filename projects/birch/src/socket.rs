use std::io::{Error, ErrorKind, Read, Result, Write};

use mio::net::TcpStream;
use mio::{Evented, Poll, PollOpt, Ready, Token};

use crate::proto::RawMessage;

pub trait IrcWriter {
    fn write_message(&mut self, msg: &RawMessage) -> Result<()>;
}

#[derive(Clone)]
pub struct IrcSocketConfig {
    // TODO: stop being lazy, make this &str
    pub addr: String,
    pub max_retries: Option<usize>,
}

const MAX_LINE_LENGTH: usize = 1024;

struct MessageBuffer {
    buf: [u8; MAX_LINE_LENGTH],
    len: usize,
}

impl MessageBuffer {
    fn new() -> Self {
        Self {
            buf: [0u8; MAX_LINE_LENGTH],
            len: 0,
        }
    }

    // TODO: This logic is complicated. Test this.
    fn extract_message(&mut self, nl_pos: usize) -> Result<RawMessage> {
        let msg = std::str::from_utf8(&self.buf[..nl_pos])
            .map(|line| line.trim_end_matches(|c| c == '\r' || c == '\n'))
            .map_err(|_| Error::new(ErrorKind::Other, "invalid UTF-8"))
            .and_then(|line| {
                RawMessage::parse(line).ok_or_else(|| {
                    Error::new(ErrorKind::Other, "failed to parse message")
                })
            });

        // "Rotate" the buffer left and clear everything.
        let end = nl_pos + 1;
        self.buf[..end].iter_mut().for_each(|b| *b = 0);
        self.buf.rotate_left(end);
        self.len -= end;

        msg
    }

    /// Continually fill buffer from `r` until an error occurs or a
    /// message is produced.
    fn read<R: Read>(&mut self, r: &mut R) -> Result<RawMessage> {
        loop {
            if let Some(nl) = self.buf.iter().position(|&c| c == b'\n') {
                return self.extract_message(nl);
            } else if self.len >= MAX_LINE_LENGTH {
                // Reached max length without seeing a newline
                return Err(Error::new(ErrorKind::Other, "max line length exceeded!"));
            }

            // No messages to be parsed yet, fill the buffer.
            self.len += r.read(&mut self.buf[self.len..])?;
        }
    }
}

pub struct Socket {
    buf: MessageBuffer,
    stream: TcpStream,
}

impl Socket {
    pub fn from_stream(stream: TcpStream) -> Result<Self> {
        Ok(Self {
            buf: MessageBuffer::new(),
            stream,
        })
    }

    pub fn read_message(&mut self) -> Result<RawMessage> {
        self.buf.read(&mut self.stream)
    }

    pub fn write_message(&mut self, msg: &RawMessage) -> Result<()> {
        let msg_str = msg.to_string();
        self.stream.write_all(msg_str.as_bytes())?;
        self.stream.write_all(b"\r\n")?;
        Ok(())
    }
}

impl Evented for Socket {
    fn register(
        &self,
        poll: &Poll,
        token: Token,
        interest: Ready,
        opts: PollOpt,
    ) -> Result<()> {
        self.stream.register(poll, token, interest, opts)
    }

    fn reregister(
        &self,
        poll: &Poll,
        token: Token,
        interest: Ready,
        opts: PollOpt,
    ) -> Result<()> {
        self.stream.reregister(poll, token, interest, opts)
    }

    fn deregister(&self, poll: &Poll) -> Result<()> {
        poll.deregister(&self.stream)
    }
}

impl IrcWriter for Socket {
    fn write_message(&mut self, msg: &RawMessage) -> Result<()> {
        let msg_str = msg.to_string();
        self.stream.write_all(msg_str.as_bytes())?;
        self.stream.write_all(b"\r\n")?;

        Ok(())
    }
}
