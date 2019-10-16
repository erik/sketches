#[macro_use]
extern crate crossbeam_channel;

use std::collections::HashMap;
use std::io::{BufReader, Error, ErrorKind, Result};
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

use crossbeam_channel::{tick, unbounded, Sender};

use birch::client::{ClientConnection, ClientEvent};
use birch::proto::RawMessage;
use birch::socket::{IrcChannel, IrcReader, IrcSocket, IrcSocketConfig, IrcWriter};

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

    fn accept_client_connection(
        &mut self,
        stream: TcpStream,
        new_user_chan: Sender<IrcChannel>,
    ) {
        let to_client = unbounded();
        let from_client = unbounded();

        let read_stream = stream.try_clone().expect("clone stream");
        let mut write_stream = stream.try_clone().expect("clone stream");

        // Read thread - handle input from client
        let client_sender = from_client.0.clone();
        thread::spawn(move || {
            // TODO: This should not be a magic number
            read_stream
                .set_read_timeout(Some(Duration::from_secs(240)))
                .expect("set_read_timeout call failed");

            let mut reader = BufReader::new(read_stream);
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

        let fanout = Arc::clone(&self.fanout_manager);
        thread::spawn(move || {
            let events = unbounded();
            let ping_ticker = tick(Duration::from_secs(120));
            let mut client = ClientConnection::new(events.0);

            let recv_err = || Error::new(ErrorKind::Other, "receiver disconnected");

            loop {
                let result = select! {
                    recv(events.1) -> msg => {
                        msg.map_err(|_| recv_err())
                            .and_then(|msg| match msg {
                                ClientEvent::WriteNetwork(msg) => {
                                    println!("todo; send to network: {:?}", msg);
                                    Ok(())
                                },
                                ClientEvent::WriteClient(msg) =>
                                    to_client.0.send(msg).map_err(|_| recv_err()),

                                ClientEvent::Authenticated => {
                                    fanout.lock().unwrap().add_client(to_client.0.clone());
                                    new_user_chan.send(IrcChannel(to_client.0.clone())).expect("blah");
                                    Ok(())
                                },
                            })
                    },

                    recv(to_client.1) -> msg => {
                        msg.map_err(|_| recv_err())
                            .and_then(|msg| write_stream.write_message(&msg))
                    },

                    recv(from_client.1) -> msg => {
                        msg.map_err(|_| recv_err())
                            .and_then(|msg| client.handle_message(&msg))
                    },

                    recv(ping_ticker) -> _ => client.ping()
                };

                if let Err(err) = result {
                    println!("client failed: {:?}", err);
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
    let mut sock = IrcSocket::new(config);

    let mut fanout_sender = client_manager.fanout_sender();
    let new_user_chan = sock.new_user_channel();

    thread::spawn(move || {
        // TODO: surface any errors here + shutdown hook
        sock.start_loop(&mut fanout_sender)
    });

    // TODO: hook this up to config
    let listener = TcpListener::bind("127.0.0.1:9123")?;
    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                println!("Client connected");
                client_manager.accept_client_connection(stream, new_user_chan.clone());
            }
            Err(err) => println!("Failed to accept: {:?}", err),
        }
    }

    Ok(())
}
