use std::io::{BufReader, ErrorKind, Result};
use std::net::SocketAddr;
use std::thread;
use std::time::Duration;

use mio::net::{TcpListener, TcpStream};
use mio::{Evented, Events, Poll, PollOpt, Ready, Registration, Token};
use slab::Slab;

use birch::client::{ClientAuth, ClientConnection, ClientEvent};
use birch::network::NetworkConnection;
use birch::socket::{IrcReader, IrcSocketConfig, IrcWriter};

// TODO: blah
type NetworkId = usize;
type ClientId = usize;

struct Socket {
    reader: BufReader<TcpStream>,
    writer: TcpStream,
}

impl Evented for Socket {
    fn register(
        &self,
        poll: &Poll,
        token: Token,
        interest: Ready,
        opts: PollOpt,
    ) -> Result<()> {
        self.writer.register(poll, token, interest, opts)
    }

    fn reregister(
        &self,
        poll: &Poll,
        token: Token,
        interest: Ready,
        opts: PollOpt,
    ) -> Result<()> {
        self.writer.reregister(poll, token, interest, opts)
    }

    fn deregister(&self, poll: &Poll) -> Result<()> {
        poll.deregister(&self.writer)
    }
}

impl Socket {
    fn from_stream(stream: TcpStream) -> Result<Self> {
        let reader = BufReader::new(stream.try_clone()?);
        Ok(Self {
            reader,
            writer: stream,
        })
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

pub struct Ticker(Registration);

impl Ticker {
    pub fn new(interval: Duration) -> Self {
        let (registration, set_readiness) = Registration::new2();

        thread::spawn(move || loop {
            thread::sleep(interval);
            set_readiness.set_readiness(Ready::readable()).unwrap();
        });

        Self(registration)
    }
}

impl Evented for Ticker {
    fn register(
        &self,
        poll: &Poll,
        token: Token,
        interest: Ready,
        opts: PollOpt,
    ) -> Result<()> {
        self.0.register(poll, token, interest, opts)
    }

    fn reregister(
        &self,
        poll: &Poll,
        token: Token,
        interest: Ready,
        opts: PollOpt,
    ) -> Result<()> {
        self.0.reregister(poll, token, interest, opts)
    }

    fn deregister(&self, poll: &Poll) -> Result<()> {
        poll.deregister(&self.0)
    }
}

struct Client {
    socket: Socket,
    conn: ClientConnection,
    network: Option<NetworkId>,
}

impl Client {
    fn new(stream: TcpStream) -> Self {
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
    auth: (String, String),
}

impl NetworkConfig {
    fn create_socket(&self) -> Result<Socket> {
        let stream =
            mio::net::TcpStream::from_stream(TcpStream::connect(&self.socket.addr)?)?;
        Socket::from_stream(stream)
    }
}

struct Network {
    socket: Socket,
    conn: NetworkConnection,
    config: NetworkConfig,
    connected: bool,
}

impl Network {
    fn new(config: NetworkConfig) -> Result<Self> {
        let socket = config.create_socket()?;
        let conn = NetworkConnection::new(&config.nick);

        Ok(Self {
            socket,
            conn,
            config,
            connected: true,
        })
    }

    fn reconnect(&mut self) -> Result<()> {
        self.socket = self.config.create_socket()?;
        self.conn.initialize()?;
        self.connected = true;

        Ok(())
    }

    // TODO: blah, this could be better
    fn authenticate(&self, auth: &ClientAuth) -> bool {
        auth.network == self.config.network_name
            && auth.user == self.config.auth.0
            && auth.password == self.config.auth.1
    }
}

#[derive(PartialEq)]
enum SocketKind {
    Ticker,
    Listener,
    Network(NetworkId),
    Client(ClientId),
}

struct BirchServer {
    sockets: Slab<SocketKind>,
    clients: Slab<Client>,
    // TODO: Since networks aren't dynamic, no benefit really to having this be a slab
    networks: Slab<Network>,
}

impl BirchServer {
    fn new() -> Self {
        Self {
            sockets: Slab::with_capacity(1024),
            clients: Slab::with_capacity(32),
            networks: Slab::with_capacity(16),
        }
    }

    fn accept_client(&mut self, poll: &Poll, socket: TcpStream) -> Result<()> {
        let client_id = self.clients.insert(Client::new(socket));
        let token = self.sockets.insert(SocketKind::Client(client_id));

        poll.register(
            &self.clients.get(client_id).unwrap().socket,
            Token(token),
            Ready::readable(),
            PollOpt::edge(),
        )?;

        println!("Client connected: client_id={} token={}", client_id, token);
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
                ClientEvent::AuthAttempt(auth) => {
                    let id = self
                        .networks
                        .iter()
                        .find(|(_, n)| n.authenticate(&auth))
                        .map(|(id, _)| id);

                    // TODO: set auth result back on client.conn
                    client.network = id
                }
            }
        }

        Ok(())
    }

    fn ping_clients(&mut self, poll: &Poll) -> Result<()> {
        let mut to_remove = Vec::new();
        for (id, client) in self.clients.iter_mut() {
            if !client.conn.ping() {
                println!("client({}) timed out", id);
                to_remove.push(id);
            }

            // TODO: remove duplication
            for event in client.conn.events() {
                match event {
                    ClientEvent::WriteClient(ref msg) => {
                        client.socket.write_message(msg)?
                    }
                    _ => unreachable!(),
                }
            }
        }

        for client_id in to_remove.iter() {
            self.remove_client(poll, *client_id)?;
        }

        Ok(())
    }

    // TODO: This is a bit too complicated
    fn remove_client(&mut self, poll: &Poll, id: ClientId) -> Result<()> {
        let client = self.clients.remove(id);
        poll.deregister(&client.socket)?;

        let (tok, _) = self
            .sockets
            .iter()
            .find(|(_, s)| SocketKind::Client(id) == **s)
            .unwrap();

        self.sockets.remove(tok);
        Ok(())
    }

    fn reconnect_networks(&mut self, poll: &Poll) -> Result<()> {
        for (id, network) in self.networks.iter_mut().filter(|(_, n)| !n.connected) {
            println!("network {} is disconnected... attempting reconnect", id);
            match network.reconnect() {
                Ok(()) => poll.register(
                    &network.socket,
                    Token(self.sockets.insert(SocketKind::Network(id))),
                    Ready::readable(),
                    PollOpt::edge(),
                )?,
                Err(err) => println!("reconnect failed: {}", err),
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
        match self.sockets.get(token.0) {
            None => panic!("token not associated with a socket: {:?}", token),

            Some(&SocketKind::Ticker) => {
                self.ping_clients(poll)?;
                self.reconnect_networks(poll)?;
            }

            Some(&SocketKind::Listener) => loop {
                match listener.accept() {
                    Ok((socket, _)) => self.accept_client(&poll, socket)?,
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
                        self.sockets.remove(token.0);
                        let mut network = self.networks.get_mut(network_id).unwrap();
                        network.connected = false;
                        poll.deregister(&network.socket)?;
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
                        self.remove_client(poll, client_id)?;
                        break;
                    }
                }
            },
        }

        Ok(())
    }

    fn add_network(&mut self, config: NetworkConfig) -> Result<NetworkId> {
        let mut network = Network::new(config)?;
        network.conn.initialize()?;

        let token = self.networks.insert(network);
        self.sockets.insert(SocketKind::Network(token));

        Ok(token)
    }

    fn serve(&mut self, bind_addr: &SocketAddr) -> Result<()> {
        let poll = Poll::new()?;

        // TODO: is this iffy? Should we just store the Poll object on
        // the struct itself?
        for (id, network) in self.networks.iter_mut() {
            poll.register(
                &network.socket,
                Token(id),
                Ready::readable(),
                PollOpt::edge(),
            )?;
        }

        let listener = TcpListener::bind(bind_addr)?;
        {
            let tok = self.sockets.insert(SocketKind::Listener);
            poll.register(&listener, Token(tok), Ready::readable(), PollOpt::edge())?;
        }

        let ticker = Ticker::new(Duration::from_secs(60));
        {
            let tok = self.sockets.insert(SocketKind::Ticker);
            poll.register(&ticker, Token(tok), Ready::readable(), PollOpt::edge())?;
        }

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
    let config = NetworkConfig {
        network_name: "freenode".to_string(),
        nick: "ep".to_string(),
        auth: ("foo".to_string(), "bar".to_string()),
        socket: IrcSocketConfig {
            addr: "irc.freenode.net:6667".parse().unwrap(),
            max_retries: Some(3),
        },
    };

    let mut server = BirchServer::new();
    server.add_network(config)?;

    server.serve(&"127.0.0.1:9123".parse().unwrap())
}
