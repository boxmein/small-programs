use crate::model::Task;
use crate::model::TaskResult;

#[derive(Debug, Clone)]
pub struct TaskAndResult {
    pub task: Task,
    pub result: TaskResult,
}

#[derive(Debug, Clone)]
pub struct RunResult {
    pub task_results: Vec<TaskAndResult>,
}
