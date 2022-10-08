use crate::model::{Config, RunResult};
use anyhow::Result;
use std::time::SystemTime;

pub trait Context {
    type LoggerType: Logger;
    fn get_logger(&self) -> &Self::LoggerType;
}

pub trait LastModifiedTimeable {
    fn get_last_modified_timestamp(&self) -> Result<SystemTime>;
}

pub trait Executable<T> {
    fn execute(&self, context: &impl Context) -> Result<T>;
}

pub trait Logger {
    fn log(&self, msg: &str);
}

pub trait Run {
    fn new(config: Config) -> Self;
    fn run(&self, ctx: &impl Context) -> Result<RunResult>;
}
