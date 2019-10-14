use std::collections::HashMap;
use std::io::BufReader;
use std::io::Result;
use std::io::{Error, ErrorKind};
use std::net::{TcpListener, TcpStream};
use std::sync::mpsc::sync_channel;
use std::sync::mpsc::{Receiver, SyncSender};
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;

use birch::client::ClientConnection;
use birch::proto::RawMessage;
use birch::socket::IrcReader;
use birch::socket::{IrcSocket, IrcSocketConfig, IrcWriter};

#[derive(Clone)]
struct IrcChannel(SyncSender<RawMessage>);

impl IrcChannel {
    fn new() -> (Self, Receiver<RawMessage>) {
        let (sender, receiver) = sync_channel(100);
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
    clients: HashMap<usize, SyncSender<RawMessage>>,
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

    fn add_client(&mut self, sender: SyncSender<RawMessage>) -> usize {
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
        let (sender, receiver) = sync_channel(100);

        // TODO: write up disconnect logic
        let _client_id = self
            .fanout_manager
            .lock()
            .unwrap()
            .add_client(sender.clone());

        let mut writer = stream.try_clone().expect("clone stream");
        // Writer thread
        thread::spawn(move || {
            for msg in receiver {
                if let Err(err) = writer.write_message(&msg) {
                    println!("failed to write to client: {:?}", err);
                    break;
                }
            }
        });

        // Reader thread
        thread::spawn(move || {
            let mut writer = IrcChannel(sender);
            let mut client = ClientConnection::new(&mut writer);
            let mut reader = BufReader::new(stream);

            loop {
                let result = reader
                    .read_message()
                    .and_then(|msg| client.handle_message(&msg));

                if result.is_err() {
                    break;
                }
            }
        });
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
