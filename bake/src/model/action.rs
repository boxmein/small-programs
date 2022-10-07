use crate::traits::{Context, Executable, Logger};
use anyhow::Result;
use serde::Deserialize;
use serde::Serialize;
use std::process::Command;
use std::process::Output;
use tracing::debug;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Action {
    Shell(String),
}

impl Action {
    fn into_action_result(&self, output: Output) -> ActionResult {
        ActionResult::new_with_output(self.clone(), output)
    }
}

impl Executable<ActionResult> for Action {
    fn execute(&self, ctx: &impl Context) -> Result<ActionResult> {
        match &self {
            &Action::Shell(ref cmd) => Ok(self.into_action_result(execute_shell(ctx, cmd)?)),
        }
    }
}

fn execute_shell(ctx: &impl Context, cmd: &str) -> Result<Output> {
    debug!("execute_shell: {cmd}", cmd = cmd);
    ctx.get_logger().log(&format!("command: bash -c '{}'", cmd));
    let child = Command::new("bash").args(["-c", cmd]).spawn()?;
    Ok(child.wait_with_output()?)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Status {
    Ok,
}

#[derive(Debug, Clone)]
pub struct ActionResult {
    pub action: Action,
    pub status: Status,
    pub output: Option<Output>,
}

impl ActionResult {
    pub fn new_with_output(action: Action, output: Output) -> ActionResult {
        ActionResult {
            action,
            status: Status::Ok,
            output: Some(output),
        }
    }
}
