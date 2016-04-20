use std::option::Option;

use redis;
use rustc_serialize::json::ToJson;

use task::{TaskDef, TaskId, TaskState};


pub trait Broker {
    fn execute_task(&self, task: &TaskDef, args: &ToJson) -> TaskId;
    fn task_status(&self, task_id: &TaskId) -> ();
    fn await(&self, task_id: &TaskId) -> TaskState;
    fn get(&self, task_id: &TaskId) -> Option<TaskState>;
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
    fn execute_task(&self, task: &TaskDef, args: &ToJson) -> TaskId {
        unimplemented!()
    }
    fn task_status(&self, task_id: &TaskId) -> () {
        unimplemented!()
    }
    fn await(&self, task_id: &TaskId) -> TaskState {
        unimplemented!()
    }
    fn get(&self, task_id: &TaskId) -> Option<TaskState> {
        unimplemented!()
    }
}
