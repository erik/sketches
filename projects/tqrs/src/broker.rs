use std::option::Option;

use redis;
use rustc_serialize::json::ToJson;
use uuid::Uuid;


use task::{TaskDef, Task, TaskState};


pub struct RedisTask<'a> {
    id: String,
    state: TaskState,
    broker: &'a RedisBroker
}


impl<'a> Task for RedisTask<'a> {
    fn status<'b>(&self) -> Option<&'b ToJson> {
        None
    }

    fn await<'b>(&self) -> Option<&'b ToJson> {
        None
    }

    fn get<'b>(&self) -> Option<&'b ToJson> {
        None
    }
}


pub struct RedisBroker {
    conn: redis::Connection,
    key_prefix: &'static str,
    poll_interval_ms: u32
}


impl RedisBroker {
    pub fn new(conn: redis::Connection) -> RedisBroker {
        RedisBroker {conn: conn, key_prefix: "", poll_interval_ms: 5}
    }

    pub fn execute_task(&self, task_def: &TaskDef, args: &ToJson) -> RedisTask {
        let id = Uuid::new_v4().simple().to_string();

        RedisTask {
            id: id,
            state: TaskState::Queued,
            broker: self
        }
    }
}
