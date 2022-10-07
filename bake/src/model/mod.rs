mod task;
pub use task::*;
mod input;
pub use input::*;
mod action;
pub use action::*;
mod output;
pub use output::*;
mod context;
pub use context::*;

pub struct Config {
    pub tasks: Vec<Task>,
}
