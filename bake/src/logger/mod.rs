use crate::traits::Logger;

pub struct StdoutLogger;

impl Logger for StdoutLogger {
    fn log(&self, msg: &str) {
        println!("{}", msg);
    }
}
