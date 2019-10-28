#![allow(dead_code)]

use std::io::{BufReader, ErrorKind, Result};
use std::net::TcpStream;

use mio::{net::TcpListener, Events, Poll, PollOpt, Ready, Token};
use slab::Slab;

use birch::client::{ClientConnection, ClientEvent};
use birch::socket::{IrcReader, IrcSocketConfig, IrcWriter};

// TODO: blah
type NetworkId = usize;

struct Client {
    reader: BufReader<mio::net::TcpStream>,
    writer: mio::net::TcpStream,
    conn: ClientConnection,
    network: Option<NetworkId>,
}

impl Client {
    fn new(stream: &mio::net::TcpStream) -> Self {
        let reader = BufReader::new(stream.try_clone().expect("fork stream"));
        let writer = stream.try_clone().expect("fork stream");

        let conn = ClientConnection::new();

        Self {
            reader,
            writer,
            conn,
            network: None,
        }
    }
}

struct NetworkConfig<'a> {
    network_name: &'a str,
    nick: &'a str,
    socket: IrcSocketConfig,
}

struct Network {
    // TODO: define this
}

enum SocketKind {
    Listener,
    Network(std::io::BufReader<mio::net::TcpStream>, mio::net::TcpStream),
    Client(Client),
}

struct TokenManager {
    tokens: Slab<SocketKind>,
}

impl TokenManager {
    fn new() -> Self {
        Self {
            tokens: Slab::with_capacity(1024),
        }
    }

    fn add_client_listener(&mut self) -> Token {
        Token(self.tokens.insert(SocketKind::Listener))
    }

    fn add_network(&mut self, stream: &mio::net::TcpStream) -> Token {
        let reader = BufReader::new(stream.try_clone().expect("fork stream"));
        let writer = stream.try_clone().expect("fork stream");

        Token(self.tokens.insert(SocketKind::Network(reader, writer)))
    }

    fn add_client(&mut self, stream: &mio::net::TcpStream) -> Token {
        let client = Client::new(stream);
        Token(self.tokens.insert(SocketKind::Client(client)))
    }

    fn find(&mut self, token: Token) -> Option<&mut SocketKind> {
        self.tokens.get_mut(token.0)
    }

    fn remove(&mut self, token: Token) -> bool {
        if self.tokens.contains(token.0) {
            self.tokens.remove(token.0);
            true
        } else {
            false
        }
    }
}

fn main() -> Result<()> {
    let poll = Poll::new()?;
    let mut token_manager = TokenManager::new();

    let config = NetworkConfig {
        network_name: "freenode",
        nick: "ep",
        socket: IrcSocketConfig {
            addr: "irc.freenode.net:6667".parse().unwrap(),
            max_retries: Some(3),
        },
    };

    let mut network = birch::network::NetworkConnection::new("ep");
    // TODO: this sucks
    network.initialize()?;

    // TODO: Wrap in retry logic
    let network_stream =
        mio::net::TcpStream::from_stream(TcpStream::connect(config.socket.addr)?)?;

    poll.register(
        &network_stream,
        token_manager.add_network(&network_stream),
        Ready::readable(),
        PollOpt::edge(),
    )?;

    let listener = TcpListener::bind(&"127.0.0.1:9123".parse().unwrap())?;
    poll.register(
        &listener,
        token_manager.add_client_listener(),
        Ready::readable(),
        PollOpt::edge(),
    )?;

    loop {
        let mut events = Events::with_capacity(128);
        poll.poll(&mut events, None)?;

        for ev in events {
            let mut remove_socket = false;

            let socket = token_manager.find(ev.token());
            match socket {
                Some(SocketKind::Listener) => loop {
                    match listener.accept() {
                        Ok((socket, _)) => {
                            let token = token_manager.add_client(&socket);

                            poll.register(
                                &socket,
                                token,
                                Ready::readable(),
                                PollOpt::edge(),
                            )?;
                        }
                        Err(ref e) if e.kind() == ErrorKind::WouldBlock => break,
                        e => panic!("err={:?}", e),
                    }
                },

                Some(SocketKind::Network(ref mut reader, writer)) => loop {
                    match reader.read_message() {
                        Ok(msg) => {
                            println!("[birch <- \u{1b}[37;1mnet\u{1b}[0m] {}", msg);
                            if let Err(err) = network.handle(&msg) {
                                println!("Failed to handle message: {}", err);
                                break;
                            }

                            for msg in network.network_messages() {
                                println!("[\u{1b}[37;1mbirch\u{1b}[0m -> net] {}", msg);
                                // TODO: handle errors
                                writer.write_message(&msg)?;
                            }

                            for msg in network.user_messages() {
                                // TODO: implement this
                                println!("fanout: {}", msg);
                            }
                        }
                        Err(ref e) if e.kind() == ErrorKind::WouldBlock => break,
                        Err(e) => {
                            // TODO: Reconnection logic
                            println!("Network errored, disconnecting: {}", e);
                            remove_socket = true;
                            break;
                        }
                    }
                },
                Some(SocketKind::Client(ref mut client)) => loop {
                    match client.reader.read_message() {
                        Ok(msg) => {
                            println!(
                                "[birch <- \u{1b}[37;1mclient({})\u{1b}[0m] {}",
                                ev.token().0,
                                msg
                            );

                            if let Err(err) = client.conn.handle_message(&msg) {
                                println!("Failed to handle message: {}", err);
                                break;
                            }

                            for event in client.conn.events() {
                                match event {
                                    ClientEvent::WriteNetwork(ref _msg) => unimplemented!(),
                                    ClientEvent::WriteClient(ref msg) => {
                                        client.writer.write_message(msg)?
                                    }
                                    ClientEvent::RegistrationComplete => {
                                        // TODO: dynamic network
                                        // TODO: Update client's nick
                                        network.state.welcome_user(&mut client.writer)?;
                                    }
                                    ClientEvent::AuthAttempt(_auth) => {
                                        // TODO: actual auth here
                                        // TODO: set auth result back on client.conn
                                        client.network = Some(0);
                                    }
                                }
                            }
                        }
                        Err(ref e) if e.kind() == ErrorKind::WouldBlock => break,
                        Err(e) => {
                            println!("Client errored, disconnecting: {}", e);
                            remove_socket = true;
                            break;
                        }
                    }
                },

                _ => unreachable!(),
            }

            // TODO: Deregister sockets.
            if remove_socket {
                token_manager.remove(ev.token());
            }
        }
    }

    Ok(())
}
