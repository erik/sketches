use std::option::Option;

use redis;
use rustc_serialize::json::ToJson;

use queue::TaskDef;

// for now
pub type TaskId = String;
pub enum TaskState {
    Queued,
    Unknown,
    Success,
    Error
}


pub struct TaskStatus {
    state: TaskState,
    result: ToJson
}


pub trait Broker {
    fn execute_task(&mut self, task: &TaskDef) -> ();
    fn task_status(&self, task_id: &TaskId) -> ();
    fn await(&self, task_id: &TaskId) -> TaskState;
    fn get(&self, task_id: &TaskId) -> Option<TaskState>;
}

struct RedisBroker {
    conn: redis::Connection,
    key_prefix: String,
    poll_interval_ms: u32
}

impl RedisBroker {

}

impl Broker for RedisBroker {
    fn execute_task(&mut self, task: &TaskDef) -> () {
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
