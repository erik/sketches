use std::io::{BufReader, Result};
use std::net::TcpStream;
use std::time::Duration;

use birch::network::NetworkConnection;
use birch::socket::IrcReader;

struct IrcSocketConfig<'a> {
    nick: &'a str,
    addr: &'a str,
    max_retries: Option<usize>,
}

struct IrcSocket<'a> {
    config: IrcSocketConfig<'a>,
}

impl<'a> IrcSocket<'a> {
    fn new(config: IrcSocketConfig<'a>) -> Self {
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
    fn connect(&self) -> Result<bool> {
        let mut stream = self.create_stream()?;
        let mut reader = BufReader::new(stream.try_clone()?);
        let mut network = NetworkConnection::new(self.config.nick, &mut stream);

        if network.initialize().is_err() {
            return Ok(true);
        }

        loop {
            let result = reader.read_message().and_then(|msg| {
                println!("[birch <- \u{1b}[37;1mnet\u{1b}[0m] {}", msg);
                network.handle(&msg)
            });

            if result.is_err() {
                println!("read failed");
                break;
            }
        }

        Ok(true)
    }

    fn start_loop(&mut self) -> Result<()> {
        loop {
            match self.connect() {
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

fn main() -> Result<()> {
    let config = IrcSocketConfig {
        nick: "ep`",
        addr: "irc.freenode.net:6667",
        max_retries: Some(3),
    };

    let mut sock = IrcSocket::new(config);
    sock.start_loop()
}
