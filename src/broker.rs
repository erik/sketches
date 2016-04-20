use std::option::Option;

use redis;
use rustc_serialize::json::ToJson;

use task::{TaskDef, Task, TaskState, RedisTask};


pub trait Broker {
    fn execute_task(&self, task: &TaskDef, args: &ToJson) -> &Task;
    fn task_status(&self, task_id: &String) -> ();
    fn await(&self, task_id: &String) -> Option<&ToJson>;
    fn get(&self, task_id: &String) -> Option<&ToJson>;
}


struct RedisBroker {
    conn: redis::Connection,
    key_prefix: &'static str,
    poll_interval_ms: u32
}


impl RedisBroker {
    fn new(conn: redis::Connection) -> RedisBroker {
        RedisBroker {conn: conn, key_prefix: "", poll_interval_ms: 5}
    }
}


impl Broker for RedisBroker {
    fn execute_task(&self, task: &TaskDef, args: &ToJson) -> &Task {
        unimplemented!()
    }

    fn task_status(&self, task_id: &String) -> () {
        unimplemented!()
    }

    fn await(&self, task_id: &String) -> Option<&ToJson> {
        unimplemented!()
    }

    fn get(&self, task_id: &String) -> Option<&ToJson> {
        unimplemented!()
    }
}
