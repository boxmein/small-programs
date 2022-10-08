mod graph;

use anyhow::Result;
use graph::GraphRun;

use crate::{
    model::{Config, RunResult},
    traits::{Context, Run},
};

pub fn get_runner(config: Config) -> impl Run {
    GraphRun::new(config)
}

pub fn run_config(config: Config, ctx: &impl Context) -> Result<RunResult> {
    let runner = get_runner(config);
    runner.run(ctx)
}
