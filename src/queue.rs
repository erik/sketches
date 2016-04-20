use std::collections::HashMap;
use std::result::Result;
use std::vec::Vec;

use rustc_serialize::json::ToJson;

use broker::Broker;


pub type TaskFunction = fn(args: ToJson) -> ToJson;


pub struct TaskDef<'a> {
    name: String,
    func: &'a TaskFunction
}


pub struct TaskRegistry<'a> {
    broker: &'a Broker,
    registry: HashMap<String, TaskDef<'a>>
}


impl <'a> TaskRegistry<'a> {
    pub fn new(broker: &Broker) -> TaskRegistry {
        TaskRegistry {
            broker: broker,
            registry: HashMap::new()
        }
    }

    pub fn add(&mut self, name: String, func: &'a TaskFunction) -> &'a mut TaskRegistry {
        self.registry.insert(name.clone(), TaskDef {
            name: name,
            func: &func
        });

        self
    }

    fn execute(&self, name: &String, args: &ToJson) -> () {
        match self.registry.get(name) {
            Some(task) => {

            },
            None => panic!("FUCK")
        }
    }
}
