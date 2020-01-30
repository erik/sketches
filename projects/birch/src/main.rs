use std::io::{ErrorKind, Result};
use std::net::SocketAddr;
use std::thread;
use std::time::Duration;

use mio::net::{TcpListener, TcpStream};
use mio::{Evented, Events, Poll, PollOpt, Ready, Registration, Token};
use rusqlite::{Connection, Result as SqlResult, NO_PARAMS};
use slab::Slab;

use birch::client::{Client, ClientEvent, ClientId};
use birch::network::{Network, NetworkConfig, NetworkId};
use birch::socket::IrcSocketConfig;

// TODO: blah

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

#[derive(PartialEq)]
enum SocketKind {
    Ticker,
    Listener,
    Network(NetworkId),
    Client(ClientId),
}

struct BirchServer {
    poll: Poll,
    sockets: Slab<SocketKind>,
    clients: Slab<Client>,
    // TODO: Since networks aren't dynamic, no benefit really to having this be a slab
    networks: Slab<Network>,
}

impl BirchServer {
    fn new() -> Self {
        Self {
            poll: Poll::new().expect("failed to create Poll instance"),
            sockets: Slab::with_capacity(128),
            clients: Slab::with_capacity(32),
            networks: Slab::with_capacity(32),
        }
    }

    fn accept_client(&mut self, stream: TcpStream) -> Result<()> {
        let entry = self.clients.vacant_entry();

        let client_id = ClientId(entry.key());
        let token = self.sockets.insert(SocketKind::Client(client_id));

        let client = Client::from_stream(stream)?;

        self.poll.register(
            &client.socket,
            Token(token),
            Ready::readable(),
            PollOpt::edge(),
        )?;

        entry.insert(client);

        println!(
            "Client connected: client_id={:?} token={}",
            client_id, token
        );
        Ok(())
    }

    fn handle_network_message(&mut self, network_id: NetworkId) -> Result<()> {
        let network = self
            .networks
            .get_mut(network_id.0)
            .expect("activity on unassigned networks id");

        let msg = network.socket.read_message()?;

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
            .get_mut(client_id.0)
            .expect("activity on unassigned client id");

        let msg = client.socket.read_message()?;

        println!(
            "[birch <- \u{1b}[37;1mclient({:?})\u{1b}[0m] {}",
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
                        Some(NetworkId(id)) => {
                            let network = self
                                .networks
                                .get_mut(id)
                                .expect("client referencing unknown network");

                            network.conn.state.welcome_client(&mut client.socket)?;
                        }
                        None => panic!("registration completed without network"),
                    }
                }
                ClientEvent::AuthAttempt(auth) => {
                    let id = self
                        .networks
                        .iter()
                        .find(|(_, n)| n.authenticate(&auth))
                        .map(|(id, _)| NetworkId(id));

                    // TODO: set auth result back on client.conn
                    client.network = id;
                }
            }
        }

        Ok(())
    }

    fn ping_clients(&mut self) -> Result<()> {
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

        for id in to_remove.iter() {
            self.remove_client(ClientId(*id))?;
        }

        Ok(())
    }

    // TODO: This is a bit too complicated
    fn remove_client(&mut self, id: ClientId) -> Result<()> {
        let client = self.clients.remove(id.0);
        self.poll.deregister(&client.socket)?;

        let (tok, _) = self
            .sockets
            .iter()
            .find(|(_, s)| SocketKind::Client(id) == **s)
            .unwrap();

        self.sockets.remove(tok);
        Ok(())
    }

    fn reconnect_networks(&mut self) -> Result<()> {
        for (id, network) in self.networks.iter_mut().filter(|(_, n)| !n.connected) {
            println!("network {} is disconnected... attempting reconnect", id);
            match network.reconnect() {
                Ok(()) => self.poll.register(
                    &network.socket,
                    Token(self.sockets.insert(SocketKind::Network(NetworkId(id)))),
                    Ready::readable(),
                    PollOpt::edge(),
                )?,
                Err(err) => println!("reconnect failed: {}", err),
            }
        }

        Ok(())
    }

    fn handle_poll_event(&mut self, listener: &TcpListener, token: Token) -> Result<()> {
        match self.sockets.get(token.0) {
            None => panic!("token not associated with a socket: {:?}", token),

            Some(&SocketKind::Ticker) => {
                self.ping_clients()?;
                self.reconnect_networks()?;
            }

            Some(&SocketKind::Listener) => loop {
                match listener.accept() {
                    Ok((socket, _)) => self.accept_client(socket)?,
                    Err(ref e) if e.kind() == ErrorKind::WouldBlock => break,
                    e => panic!("failed to accept err={:?}", e),
                }
            },

            Some(&SocketKind::Network(network_id)) => loop {
                match self.handle_network_message(network_id) {
                    Ok(_) => (),
                    Err(ref e) if e.kind() == ErrorKind::WouldBlock => break,
                    Err(e) => {
                        println!("Network errored, disconnecting: {}", e);
                        self.sockets.remove(token.0);
                        let mut network = self.networks.get_mut(network_id.0).unwrap();
                        network.connected = false;
                        self.poll.deregister(&network.socket)?;
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
                        self.remove_client(client_id)?;
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

        let entry = self.networks.vacant_entry();
        let network_id = NetworkId(entry.key());
        let id = self.sockets.insert(SocketKind::Network(network_id));

        self.poll.register(
            &network.socket,
            Token(id),
            Ready::readable(),
            PollOpt::edge(),
        )?;

        entry.insert(network);
        Ok(network_id)
    }

    fn serve(&mut self, bind_addr: &SocketAddr) -> Result<()> {
        let listener = TcpListener::bind(bind_addr)?;
        {
            let tok = self.sockets.insert(SocketKind::Listener);
            self.poll.register(
                &listener,
                Token(tok),
                Ready::readable(),
                PollOpt::edge(),
            )?;
        }

        {
            let ticker = Ticker::new(Duration::from_secs(60));
            let tok = self.sockets.insert(SocketKind::Ticker);
            self.poll.register(
                &ticker,
                Token(tok),
                Ready::readable(),
                PollOpt::edge(),
            )?;
        }

        loop {
            let mut events = Events::with_capacity(128);
            self.poll.poll(&mut events, None)?;

            for ev in events {
                self.handle_poll_event(&listener, ev.token())?;
            }
        }
    }
}

struct Database(Connection);

impl Database {
    fn new(path: &str) -> Self {
        let conn = Connection::open(&path).expect("failed to create database");

        let mut db = Self(conn);

        db.auto_migrate().expect("migration failed");

        db
    }
    fn auto_migrate(&mut self) -> SqlResult<()> {
        self.0.execute_batch(include_str!("sql/schema/base.sql"))
    }

    fn fetch_networks(&mut self) -> SqlResult<Vec<NetworkConfig>> {
        let mut stmt = self.0.prepare(include_str!("sql/query/get_networks.sql"))?;
        let mut rows = stmt.query(NO_PARAMS)?;

        let mut configs = Vec::new();
        while let Some(row) = rows.next()? {
            configs.push(NetworkConfig {
                network_name: row.get(0)?,
                nick: row.get(1)?,
                // TODO: Change how auth works so we don't hard code this.
                auth: ("foo".to_string(), "bar".to_string()),
                socket: IrcSocketConfig {
                    addr: row.get::<usize, String>(2)?.parse().unwrap(),
                    // TODO: Should this be configurable?
                    max_retries: Some(3),
                },
            })
        }

        Ok(configs)
    }
}

// TODO: Want to add CLI-style interface here so that we can run
// `birch create` to add configurations etc.
fn main() -> Result<()> {
    // TODO: Database URL should be configurable
    let mut db = Database::new("/tmp/birch.sqlite");

    let mut server = BirchServer::new();

    let configs = db.fetch_networks().expect("TODO: map error types");
    if configs.is_empty() {
        println!("No network configurations, nothing to do!");
        return Ok(());
    }

    for config in configs.into_iter() {
        println!("Adding configuration: {:?}", config);
        server.add_network(config)?;
    }

    server.serve(&"127.0.0.1:9123".parse().unwrap())
}
