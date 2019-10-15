#[macro_use]
extern crate crossbeam;

use std::collections::HashMap;
use std::io::BufReader;
use std::io::Result;
use std::io::{Error, ErrorKind};
use std::net::{Shutdown, TcpListener, TcpStream};
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;
use std::time::Duration;

use crossbeam::crossbeam_channel::{tick, unbounded, Receiver, Sender};

use birch::client::ClientConnection;
use birch::proto::RawMessage;
use birch::socket::IrcReader;
use birch::socket::{IrcSocket, IrcSocketConfig, IrcWriter};

#[derive(Clone)]
struct IrcChannel(Sender<RawMessage>);

impl IrcChannel {
    fn new() -> (Self, Receiver<RawMessage>) {
        let (sender, receiver) = unbounded();
        (IrcChannel(sender), receiver)
    }
}

impl IrcWriter for IrcChannel {
    fn write_message(&mut self, msg: &RawMessage) -> Result<()> {
        if let Err(err) = self.0.send(msg.clone()) {
            println!("Failed to write to client: {:?}", err);
            return Err(Error::new(ErrorKind::Other, "other end disconnected"));
        }
        Ok(())
    }
}

struct ClientFanoutManager {
    id_counter: usize,
    clients: HashMap<usize, Sender<RawMessage>>,
}

impl ClientFanoutManager {
    fn new() -> Self {
        Self {
            id_counter: 0,
            clients: HashMap::new(),
        }
    }

    fn next_id(&mut self) -> usize {
        let id = self.id_counter;
        self.id_counter += 1;
        id
    }

    fn add_client(&mut self, sender: Sender<RawMessage>) -> usize {
        let id = self.next_id();
        self.clients.insert(id, sender);
        id
    }

    fn remove_client(&mut self, id: usize) -> Option<()> {
        self.clients.remove(&id).map(|_| ())
    }

    fn write_message(&mut self, msg: &RawMessage) {
        let mut dead_clients = vec![];
        for (id, client) in self.clients.iter() {
            if let Err(err) = client.send(msg.clone()) {
                println!("Failed to write to client: {:?}", err);
                dead_clients.push(*id);
            }
        }
        // Prune anything we failed to write to
        for id in dead_clients.into_iter() {
            self.remove_client(id);
        }
    }
}

struct ClientManager {
    fanout_manager: Arc<Mutex<ClientFanoutManager>>,
    fanout: IrcChannel,
}

impl ClientManager {
    fn new() -> Self {
        let (sender, receiver) = IrcChannel::new();
        let manager = Arc::new(Mutex::new(ClientFanoutManager::new()));

        let fanout = Arc::clone(&manager);
        thread::spawn(move || {
            for msg in receiver {
                fanout.lock().expect("mutex poisoned").write_message(&msg);
            }
        });

        Self {
            fanout_manager: manager,
            fanout: sender,
        }
    }

    fn fanout_sender(&self) -> IrcChannel {
        self.fanout.clone()
    }

    fn accept_client_connection(&mut self, stream: TcpStream) {
        let to_client = unbounded();
        let from_client = unbounded();

        // TODO: write up disconnect logic
        let _client_id = self
            .fanout_manager
            .lock()
            .unwrap()
            .add_client(to_client.0.clone());

        let reader = stream.try_clone().expect("clone stream");
        let mut writer = stream.try_clone().expect("clone stream");

        // Read thread - handle input from client
        let client_sender = from_client.0.clone();
        thread::spawn(move || {
            reader
                .set_read_timeout(Some(Duration::from_secs(120)))
                .expect("set_read_timeout call failed");

            let mut reader = BufReader::new(reader);

            loop {
                match reader.read_message() {
                    Ok(msg) => {
                        if let Err(err) = client_sender.send(msg) {
                            println!("Failed to send: {:?}", err);
                            break;
                        }
                    }
                    Err(err) => {
                        println!("Failed to read client message: {:?}", err);
                        break;
                    }
                }
            }
        });

        let mut channel = IrcChannel(to_client.0.clone());
        let mut client = ClientConnection::new(&mut channel);
        let ping_ticker = tick(Duration::from_secs(120));

        loop {
            select! {
                recv(to_client.1) -> msg => match msg {
                    Ok(msg) => if let Err(err) = writer.write_message(&msg) {
                        println!("failed to write to client: {:?}", err);
                        break;
                    },
                    Err(err) => {
                        println!("receive failed: {:?}", err);
                        break;
                    }
                },

                recv(from_client.1) -> msg => match msg {
                    Ok(msg) => if let Err(err) = client.handle_message(&msg) {
                        println!("failed to handle client message {:?}", err);
                        break;
                    },
                    Err(err) => {
                        println!("receive failed: {:?}", err);
                        break;
                    }
                },

                recv(ping_ticker) -> _ => if let Err(err) = client.ping() {
                    println!("Ping failed, disconnecting client: {:?}", err);
                    break;
                }
            }
        }

        stream
            .shutdown(Shutdown::Both)
            .expect("shutdown call failed");
    }
}

fn main() -> Result<()> {
    let mut client_manager = ClientManager::new();

    let config = IrcSocketConfig {
        nick: "ep",
        addr: "irc.freenode.net:6667",
        max_retries: Some(3),
    };

    let mut fanout_sender = client_manager.fanout_sender();
    let mut sock = IrcSocket::new(config);
    thread::spawn(move || {
        // TODO: surface any errors here + shutdown hook
        sock.start_loop(&mut fanout_sender)
    });

    // TODO: hook this up to config
    let listener = TcpListener::bind("127.0.0.1:9123").unwrap();
    for stream in listener.incoming() {
        if let Ok(stream) = stream {
            println!("Client connected");
            client_manager.accept_client_connection(stream);
        }
    }

    Ok(())
}
