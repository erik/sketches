use std::io::Result;

use birch::socket::{IrcSocket, IrcSocketConfig};

fn main() -> Result<()> {
    let config = IrcSocketConfig {
        nick: "ep`",
        addr: "irc.freenode.net:6667",
        max_retries: Some(3),
    };

    let mut sock = IrcSocket::new(config);
    sock.start_loop()
}
