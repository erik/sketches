use std::io::{BufReader, Write};
use std::net::{TcpListener, TcpStream};
use std::thread;
use std::time::Duration;

use birch::network::NetworkConnection;
use birch::socket::IRCReader;

fn main() {
    // Hardcoding things for now just to test everything out.
    let mut stream = TcpStream::connect("irc.freenode.net:6667").unwrap();
    stream
        .set_read_timeout(Some(Duration::from_secs(180)))
        .unwrap();

    thread::spawn(move || {
        let mut reader = BufReader::new(stream.try_clone().unwrap());
        let mut net = NetworkConnection::new("ep`", &mut stream);
        net.initialize().unwrap();

        loop {
            let msg = reader.read_message();
            if let Ok(msg) = msg {
                println!("[birch <- \u{1b}[37;1mnet\u{1b}[0m] {}", msg);
                net.handle(&msg).expect("failed to handle message");
            } else {
                println!("read failed");
                break;
            }
        }
    });

    // TODO: hook this up
    let listener = TcpListener::bind("127.0.0.1:9123").unwrap();
    println!("listening started, ready to accept");

    for stream in listener.incoming() {
        thread::spawn(|| {
            let mut stream = stream.unwrap();
            stream.write_all(b"Hello World\r\n").unwrap();
        });
    }
}
