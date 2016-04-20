#![allow(dead_code, unused_variables)]

extern crate redis;
extern crate rustc_serialize;

mod task;
mod broker;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
