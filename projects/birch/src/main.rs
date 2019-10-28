use std::io::{BufReader, ErrorKind, Result};
use std::net::TcpStream;

use mio::{net::TcpListener, Events, Poll, PollOpt, Ready, Token};
use slab::Slab;

use birch::client::{ClientConnection, ClientEvent};
use birch::socket::{IrcReader, IrcSocketConfig, IrcWriter};

// TODO: blah
type NetworkId = usize;
type ClientId = usize;

struct Socket {
    reader: BufReader<mio::net::TcpStream>,
    writer: mio::net::TcpStream,
}

impl Socket {
    fn from_stream(stream: &mio::net::TcpStream) -> Result<Self> {
        let reader = BufReader::new(stream.try_clone()?);
        let writer = stream.try_clone()?;
        Ok(Self { reader, writer })
    }
}

impl std::io::Read for Socket {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        self.reader.read(buf)
    }
}

impl std::io::Write for Socket {
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        self.writer.write(buf)
    }

    fn flush(&mut self) -> Result<()> {
        self.writer.flush()
    }
}

struct Client {
    socket: Socket,
    conn: ClientConnection,
    network: Option<NetworkId>,
}

impl Client {
    fn new(stream: &mio::net::TcpStream) -> Self {
        let socket = Socket::from_stream(stream).expect("create client socket");
        let conn = ClientConnection::new();

        Self {
            socket,
            conn,
            network: None,
        }
    }
}

struct ClientManager {
    clients: Slab<(ClientId, Client)>,
}

impl ClientManager {
    fn new() -> Self {
        Self {
            clients: Slab::with_capacity(32),
        }
    }

    fn add(&mut self, client: Client) -> ClientId {
        let entry = self.clients.vacant_entry();
        let key = entry.key();
        entry.insert((key, client));

        key
    }

    fn find(&mut self, id: ClientId) -> Option<&mut Client> {
        self.clients.get_mut(id).map(|c| &mut c.1)
    }

    fn remove(&mut self, id: ClientId) -> Option<Client> {
        if !self.clients.contains(id) {
            return None;
        }

        Some(self.clients.remove(id).1)
    }

    // TODO: use an iterator
    fn with_network(&mut self, id: NetworkId) -> Vec<&mut Client> {
        self.clients
            .iter_mut()
            .map(|(_, (_, client))| client)
            .filter(|client| client.network.is_some())
            // TODO: .filter(|client| Some(id) == client.network)
            .collect()
    }
}

struct NetworkConfig<'a> {
    network_name: &'a str,
    nick: &'a str,
    socket: IrcSocketConfig,
}

struct Network<'a> {
    id: NetworkId,
    config: NetworkConfig<'a>,
    network: birch::network::NetworkConnection,
    socket: Socket,
}

enum SocketKind {
    Listener,
    Network(std::io::BufReader<mio::net::TcpStream>, mio::net::TcpStream),
    Client(ClientId),
}

struct SocketManager {
    sockets: Slab<SocketKind>,
}

impl SocketManager {
    fn new() -> Self {
        Self {
            sockets: Slab::with_capacity(1024),
        }
    }

    fn add(&mut self, socket: SocketKind) -> Token {
        Token(self.sockets.insert(socket))
    }

    fn add_client_listener(&mut self) -> Token {
        Token(self.sockets.insert(SocketKind::Listener))
    }

    fn add_network(&mut self, stream: &mio::net::TcpStream) -> Token {
        let reader = BufReader::new(stream.try_clone().expect("fork stream"));
        let writer = stream.try_clone().expect("fork stream");

        Token(self.sockets.insert(SocketKind::Network(reader, writer)))
    }

    fn find(&mut self, token: Token) -> Option<&mut SocketKind> {
        self.sockets.get_mut(token.0)
    }

    fn remove(&mut self, token: Token) -> bool {
        if self.sockets.contains(token.0) {
            self.sockets.remove(token.0);
            true
        } else {
            false
        }
    }
}

fn main() -> Result<()> {
    let poll = Poll::new()?;
    let mut socket_manager = SocketManager::new();
    let mut client_manager = ClientManager::new();

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
        socket_manager.add_network(&network_stream),
        Ready::readable(),
        PollOpt::edge(),
    )?;

    let listener = TcpListener::bind(&"127.0.0.1:9123".parse().unwrap())?;
    poll.register(
        &listener,
        socket_manager.add_client_listener(),
        Ready::readable(),
        PollOpt::edge(),
    )?;

    loop {
        let mut events = Events::with_capacity(128);
        poll.poll(&mut events, None)?;

        for ev in events {
            let mut remove_socket = false;

            let socket = socket_manager.find(ev.token());
            match socket {
                Some(SocketKind::Listener) => loop {
                    match listener.accept() {
                        Ok((ref socket, _)) => {
                            let client_id = client_manager.add(Client::new(socket));
                            let token = socket_manager.add(SocketKind::Client(client_id));

                            poll.register(
                                socket,
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
                                // TODO: implement this correctly
                                for client in client_manager.with_network(0) {
                                    client.socket.write_message(&msg)?;
                                }
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
                Some(SocketKind::Client(client_id)) => {
                    let client = client_manager
                        .find(*client_id)
                        .expect("activity on unassigned client id");

                    loop {
                        match client.socket.reader.read_message() {
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
                                            client.socket.write_message(msg)?
                                        }
                                        ClientEvent::RegistrationComplete => {
                                            // TODO: dynamic network
                                            // TODO: Update client's nick
                                            network
                                                .state
                                                .welcome_user(&mut client.socket)?;
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
                    }
                }

                _ => unreachable!(),
            }

            // TODO: Deregister sockets.
            if remove_socket {
                socket_manager.remove(ev.token());
            }
        }
    }

    Ok(())
}
