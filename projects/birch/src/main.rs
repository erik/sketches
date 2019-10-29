use std::io::{BufReader, ErrorKind, Result};
use std::net::{SocketAddr, TcpStream};

use mio::{net::TcpListener, Events, Poll, PollOpt, Ready, Token};
use slab::Slab;

use birch::client::{ClientConnection, ClientEvent};
use birch::network::NetworkConnection;
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

struct NetworkConfig {
    network_name: String,
    nick: String,
    socket: IrcSocketConfig,
}

struct Network {
    socket: Socket,
    conn: NetworkConnection,
}

impl Network {
    fn new(config: &NetworkConfig) -> Result<Self> {
        // TODO: Wrap in retry logic
        let stream =
            mio::net::TcpStream::from_stream(TcpStream::connect(&config.socket.addr)?)?;

        let socket = Socket::from_stream(&stream).expect("create network socket");
        let conn = NetworkConnection::new(&config.nick);

        Ok(Self { socket, conn })
    }
}

enum SocketKind {
    Listener,
    Network(NetworkId),
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

    fn find(&self, token: Token) -> Option<&SocketKind> {
        self.sockets.get(token.0)
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

struct BirchServer {
    sockets: SocketManager,
    clients: Slab<Client>,
    // TODO: Since networks aren't dynamic, no benefit really to having this be a slab
    networks: Slab<Network>,
}

impl BirchServer {
    fn new() -> Self {
        Self {
            sockets: SocketManager::new(),
            clients: Slab::with_capacity(32),
            networks: Slab::with_capacity(16),
        }
    }

    fn accept_client(&mut self, poll: &Poll, socket: &mio::net::TcpStream) -> Result<()> {
        let client_id = self.clients.insert(Client::new(socket));
        let token = self.sockets.add(SocketKind::Client(client_id));

        poll.register(socket, token, Ready::readable(), PollOpt::edge())?;
        Ok(())
    }

    fn handle_network_message(&mut self, network_id: NetworkId) -> Result<()> {
        let network = self
            .networks
            .get_mut(network_id)
            .expect("activity on unassigned networks id");

        let msg = network.socket.reader.read_message()?;

        println!("[birch <- \u{1b}[37;1mnet\u{1b}[0m] {}", msg);
        network.conn.handle(&msg)?;

        for msg in network.conn.network_messages() {
            println!("[\u{1b}[37;1mbirch\u{1b}[0m -> net] {}", msg);
            network.socket.write_message(&msg)?;
        }

        for msg in network.conn.user_messages() {
            let clients = self
                .clients
                .iter_mut()
                .filter(|(_, c)| c.network == Some(network_id));

            for (_, client) in clients {
                if let Err(err) = client.socket.write_message(&msg) {
                    println!("Failed to write to client: {}", err);
                }
            }
        }

        Ok(())
    }

    fn handle_client_message(&mut self, client_id: ClientId) -> Result<()> {
        let client = self
            .clients
            .get_mut(client_id)
            .expect("activity on unassigned client id");

        let msg = client.socket.reader.read_message()?;

        println!(
            "[birch <- \u{1b}[37;1mclient({})\u{1b}[0m] {}",
            client_id, msg
        );

        client.conn.handle_message(&msg)?;

        for event in client.conn.events() {
            match event {
                ClientEvent::WriteNetwork(ref _msg) => unimplemented!(),
                ClientEvent::WriteClient(ref msg) => client.socket.write_message(msg)?,
                ClientEvent::RegistrationComplete => {
                    // TODO: Update client's nick
                    match client.network {
                        Some(id) => {
                            let network = self
                                .networks
                                .get_mut(id)
                                .expect("client referencing unknown network");

                            network.conn.state.welcome_user(&mut client.socket)?;
                        }
                        None => panic!("registration completed without network"),
                    }
                }
                ClientEvent::AuthAttempt(_auth) => {
                    let id = self.networks.iter().find(|n| true).map(|(id, _)| id);
                    // TODO: actual auth here
                    // TODO: set auth result back on client.conn
                    client.network = id
                }
            }
        }

        Ok(())
    }

    fn handle_poll_event(
        &mut self,
        poll: &Poll,
        listener: &TcpListener,
        token: Token,
    ) -> Result<()> {
        let mut remove_socket = false;

        match self.sockets.find(token) {
            None => panic!("token not associated with a socket: {:?}", token),

            Some(&SocketKind::Listener) => loop {
                match listener.accept() {
                    Ok((ref socket, _)) => self.accept_client(&poll, socket)?,
                    Err(ref e) if e.kind() == ErrorKind::WouldBlock => break,
                    e => panic!("failed to accept err={:?}", e),
                }
            },

            Some(&SocketKind::Network(network_id)) => loop {
                match self.handle_network_message(network_id) {
                    Ok(_) => (),
                    Err(ref e) if e.kind() == ErrorKind::WouldBlock => break,
                    Err(e) => {
                        // TODO: Reconnection logic
                        println!("Network errored, disconnecting: {}", e);
                        remove_socket = true;
                        break;
                    }
                }
            },
            Some(&SocketKind::Client(client_id)) => loop {
                match self.handle_client_message(client_id) {
                    Ok(_) => (),
                    Err(ref e) if e.kind() == ErrorKind::WouldBlock => break,
                    Err(e) => {
                        println!("Client errored, disconnecting: {}", e);
                        remove_socket = true;
                        break;
                    }
                }
            },
        }

        // TODO: Deregister sockets (remove from managers etc)
        if remove_socket {
            self.sockets.remove(token);
        }

        Ok(())
    }

    fn add_network(&mut self, config: &NetworkConfig) -> Result<NetworkId> {
        let mut network = Network::new(config)?;
        network.conn.initialize()?;

        let token = self.networks.insert(network);
        self.sockets.add(SocketKind::Network(token));

        Ok(token)
    }

    fn serve(&mut self, bind_addr: &SocketAddr) -> Result<()> {
        let poll = Poll::new()?;

        // TODO: is this iffy? Should we just store the Poll object on
        // the struct itself?
        for (id, network) in self.networks.iter_mut() {
            poll.register(
                &network.socket.writer,
                Token(id),
                Ready::readable(),
                PollOpt::edge(),
            )?;
        }

        let listener = TcpListener::bind(bind_addr)?;
        poll.register(
            &listener,
            self.sockets.add(SocketKind::Listener),
            Ready::readable(),
            PollOpt::edge(),
        )?;

        loop {
            let mut events = Events::with_capacity(128);
            poll.poll(&mut events, None)?;

            for ev in events {
                self.handle_poll_event(&poll, &listener, ev.token())?;
            }
        }
    }
}

fn main() -> Result<()> {
    let config = &NetworkConfig {
        network_name: "freenode".to_string(),
        nick: "ep".to_string(),
        socket: IrcSocketConfig {
            addr: "irc.freenode.net:6667".parse().unwrap(),
            max_retries: Some(3),
        },
    };

    let mut server = BirchServer::new();
    server.add_network(config)?;

    server.serve(&"127.0.0.1:9123".parse().unwrap())?;

    Ok(())
}
