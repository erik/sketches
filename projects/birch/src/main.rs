use std::io::BufReader;
use std::io::Result;
use std::io::Write;
use std::net::{TcpListener, TcpStream};
use std::sync::mpsc::sync_channel;
use std::sync::mpsc::SyncSender;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;

use birch::socket::IrcReader;
use birch::socket::{IrcSocket, IrcSocketConfig, IrcWriter};

#[derive(Clone)]
struct UserFanoutWriter {
    // TODO: Can we avoid sending strings?
    port: SyncSender<String>,
}

impl IrcWriter for UserFanoutWriter {
    fn write_raw(&mut self, msg: &str) -> Result<()> {
        self.port
            .send(msg.to_string())
            .expect("receiver disconnected");

        println!("sent: {}", msg);
        Ok(())
    }
}

fn main() -> Result<()> {
    let (sync_sender, receiver) = sync_channel(100);
    let mut user_fanout = UserFanoutWriter { port: sync_sender };

    thread::spawn(move || {
        let config = IrcSocketConfig {
            nick: "ep`",
            addr: "irc.freenode.net:6667",
            max_retries: Some(3),
        };

        let mut sock = IrcSocket::new(config);
        // TODO: surface any errors here + shutdown hook
        sock.start_loop(&mut user_fanout)
    });

    let clients = Arc::new(Mutex::new(Vec::<TcpStream>::new()));
    let mut _clients = Arc::clone(&clients);
    thread::spawn(move || {
        for line in receiver {
            println!("received message: {}", line);
            let line = line + "\r\n";

            let mut clients = _clients.lock().expect("mutex poisoned");
            let mut dead_clients = vec![];

            for (i, mut c) in clients.iter().enumerate() {
                if let Err(err) = c.write_all(line.as_bytes()) {
                    println!("Failed to write to client: {:?}", err);
                    dead_clients.push(i);
                }
            }

            // Prune anything we failed to write to
            for index in dead_clients.into_iter().rev() {
                clients.remove(index);
            }
        }
    });

    // TODO: hook this up
    let listener = TcpListener::bind("127.0.0.1:9123").unwrap();
    for stream in listener.incoming() {
        if let Ok(stream) = stream {
            println!("Client connected");
            let forked_stream = stream.try_clone().expect("clone stream");
            clients.lock().expect("mutex poisoned").push(forked_stream);

            thread::spawn(move || {
                let mut reader = BufReader::new(stream);
                loop {
                    let result = reader.read_message();
                    // TODO: Route messages to network handler
                    println!("From client: {:?}", result);
                    if result.is_err() {
                        break;
                    }
                }
            });
        }
    }

    Ok(())
}
