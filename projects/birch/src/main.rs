use std::io::{BufReader, Result};
use std::net::TcpStream;
use std::time::Duration;

use birch::network::NetworkConnection;
use birch::socket::IrcReader;

struct IrcSocket<'a> {
    stream_factory: &'a dyn Fn(usize) -> Result<TcpStream>,
}

impl<'a> IrcSocket<'a> {
    fn new(stream_factory: &'a dyn Fn(usize) -> Result<TcpStream>) -> Self {
        Self { stream_factory }
    }

    fn start(&mut self) -> Result<()> {
        let factory = self.stream_factory;

        loop {
            let mut stream = factory(0)?;
            let mut reader = BufReader::new(stream.try_clone()?);
            let mut network = NetworkConnection::new("ep`", &mut stream);

            network.initialize()?;
            loop {
                let msg = reader.read_message();
                if let Ok(msg) = msg {
                    println!("[birch <- \u{1b}[37;1mnet\u{1b}[0m] {}", msg);
                    network.handle(&msg)?;
                } else {
                    println!("read failed");
                    break;
                }
            }
        }

        Ok(())
    }
}

fn main() -> Result<()> {
    let stream_factory = |_attempt| {
        // Hardcoding things for now just to test everything out.
        let stream = TcpStream::connect("irc.freenode.net:6667")?;
        stream.set_read_timeout(Some(Duration::from_secs(180)))?;

        Ok(stream)
    };

    let mut sock = IrcSocket::new(&stream_factory);
    sock.start()
}
