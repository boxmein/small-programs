mod files;
mod logger;
mod model;
mod run;
mod traits;
mod util;

use model::{Action, Input, Output, Task};
use traits::Executable;

pub fn load_file(_filename: &str) -> model::Config {
    model::Config {
        tasks: vec![
            Task {
                inputs: vec![Input::File("src/main.rs".to_owned())],
                actions: vec![Action::Shell("cargo build".to_owned())],
                outputs: vec![Output::File("target/debug/tooling".to_owned())],
            },
            Task {
                inputs: vec![Input::File("Cargo.toml".to_owned())],
                actions: vec![Action::Shell("echo HELLO".to_owned())],
                outputs: vec![Output::File("src/main.rs".to_owned())],
            },
        ],
    }
}

pub fn exec_linearly(conf: &model::Config) {
    let ctx = model::DefaultContext {
        logger: logger::StdoutLogger {},
    };

    for task in &conf.tasks {
        let task_result = task.execute(&ctx).expect("task failed");

        println!("task done: {:?}", task);
        for result in task_result.result {
            println!("{:?}", result)
        }
    }
}

pub fn exec_graph(conf: &model::Config) {
    let ctx = model::DefaultContext {
        logger: logger::StdoutLogger {},
    };
    run::execute_tasks(conf, &ctx);
}
