use std::io::prelude::*;
use std::io::{BufRead, BufReader, Error, ErrorKind, Result};
use std::net::TcpStream;
use std::thread;
use std::time::Duration;

use crossbeam_channel::{unbounded, Receiver, Sender};

use crate::network::NetworkConnection;
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
pub struct IrcChannel(pub Sender<RawMessage>);

impl IrcChannel {
    pub fn new() -> (Self, Receiver<RawMessage>) {
        let (sender, receiver) = unbounded();
        (IrcChannel(sender), receiver)
    }
}

impl IrcWriter for IrcChannel {
    fn write_message(&mut self, msg: &RawMessage) -> Result<()> {
        if let Err(err) = self.0.send(msg.clone()) {
            println!("Failed to write to client: {:?}", err);
            return Err(Error::new(ErrorKind::Other, "other end disconnected"));
        }
        Ok(())
    }
}

pub struct IrcSocketConfig<'a> {
    pub nick: &'a str,
    pub addr: &'a str,
    pub max_retries: Option<usize>,
}

pub struct IrcSocket<'a> {
    config: IrcSocketConfig<'a>,

    to_network: (Sender<RawMessage>, Receiver<RawMessage>),
    to_users: (Sender<RawMessage>, Receiver<RawMessage>),
    from_network: (Sender<RawMessage>, Receiver<RawMessage>),
    new_user: (Sender<IrcChannel>, Receiver<IrcChannel>),
}

impl<'a> IrcSocket<'a> {
    pub fn new(config: IrcSocketConfig<'a>) -> Self {
        Self {
            config,

            to_network: unbounded(),
            to_users: unbounded(),
            from_network: unbounded(),
            new_user: unbounded(),
        }
    }

    pub fn network_channel(&self) -> Sender<RawMessage> {
        self.to_network.0.clone()
    }

    pub fn new_user_channel(&self) -> Sender<IrcChannel> {
        self.new_user.0.clone()
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
        let from_net_sender = self.from_network.0.clone();

        let mut network = NetworkConnection::new(
            self.config.nick,
            self.to_network.0.clone(),
            self.to_users.0.clone(),
        );

        let stream = self.create_stream()?;
        let mut write_stream = stream.try_clone().expect("clone failed");

        let recv_err = || Error::new(ErrorKind::Other, "receiver disconnected");

        thread::spawn(move || {
            let mut reader = BufReader::new(stream);
            loop {
                let result = reader
                    .read_message()
                    .and_then(|msg| from_net_sender.send(msg).map_err(|_| recv_err()));

                if let Err(err) = result {
                    println!("Read from network failed: {:?}", err);
                    break;
                }
            }
        });

        if network.initialize().is_err() {
            return Ok(true);
        }

        loop {
            let result = crossbeam_channel::select! {
                recv(self.from_network.1) -> msg => {
                    msg.map_err(|_| recv_err())
                        .and_then(|msg| {
                            println!("[birch <- \u{1b}[37;1mnet\u{1b}[0m] {}", msg);
                            network.handle(&msg)
                        })
                },
                recv(self.to_network.1) -> msg => {
                    msg.map_err(|_| recv_err())
                        .and_then(|msg| {
                            println!("[\u{1b}[37;1mbirch\u{1b}[0m -> net] {}", msg);
                            write_stream.write_message(&msg)
                        })
                },
                recv(self.to_users.1) -> msg => {
                    msg.map_err(|_| recv_err()).and_then(|msg| {
                        users.write_message(&msg)
                    })
                },
                recv(self.new_user.1) -> chan => {
                    chan.map_err(|_| recv_err()).and_then(|mut chan| {
                        network.state.welcome_user(&mut chan)
                    })
                }
            };

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
