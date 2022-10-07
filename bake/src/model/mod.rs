mod task;
use serde::{Deserialize, Serialize};
pub use task::*;

mod input;
pub use input::*;

mod action;
pub use action::*;

mod output;
pub use output::*;

mod runresult;
pub use runresult::*;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub tasks: Vec<Task>,
}
