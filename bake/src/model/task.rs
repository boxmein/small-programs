use crate::model::{Action, ActionResult, Input, Output};
use crate::traits::{Context, Executable};
use crate::util::get_max_mtime;
use anyhow::Result;

#[derive(Clone, Debug)]
pub struct Task {
    pub inputs: Vec<Input>,
    pub actions: Vec<Action>,
    pub outputs: Vec<Output>,
}

impl Task {
    pub fn depends_on(&self, other: &Task) -> bool {
        for input in &self.inputs {
            for output in &other.outputs {
                if input.is_provided_by(output) {
                    return true;
                }
            }
        }
        return false;
    }

    fn should_execute(&self) -> Result<bool> {
        self.output_is_older_than_input()
    }

    fn output_is_older_than_input(&self) -> Result<bool> {
        let input_age = get_max_mtime(&self.inputs)?;
        let output_age = get_max_mtime(&self.outputs)?;

        let output_is_older = input_age > output_age;

        if output_is_older {
            println!("output is older: {:?}", output_is_older);
        }

        Ok(output_is_older)
    }

    fn run_commands(&self, ctx: &impl Context) -> Result<Vec<ActionResult>> {
        self.actions.iter().map(|item| item.execute(ctx)).collect()
    }
}

impl Executable<TaskResult> for Task {
    fn execute(&self, ctx: &impl Context) -> Result<TaskResult> {
        if !self.should_execute()? {
            return Ok(TaskResult::new_nothing_to_be_done(self.clone()));
        }
        Ok(TaskResult::new_with_results(
            self.clone(),
            self.run_commands(ctx)?,
        ))
    }
}

#[derive(Debug)]
pub struct TaskResult {
    pub task: Task,
    pub result: Vec<ActionResult>,
}

impl TaskResult {
    fn new_nothing_to_be_done(task: Task) -> TaskResult {
        TaskResult {
            task,
            result: vec![],
        }
    }

    fn new_with_results(task: Task, result: Vec<ActionResult>) -> TaskResult {
        TaskResult { task, result }
    }
}
