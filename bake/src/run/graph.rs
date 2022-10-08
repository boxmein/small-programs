use crate::model::{RunResult, TaskAndResult};
use crate::traits::Run;
use crate::{
    model::{Config, Task},
    traits::{Context, Executable},
};
use anyhow::Result;
use petgraph::graph::DiGraph;
use std::collections::HashMap;
use tracing::debug;

pub struct GraphRun {
    config: Config,
}

impl GraphRun {
    fn get_graph(&self) -> Result<DiGraph<&Task, ()>> {
        let mut graph = DiGraph::new();

        let mut task_to_node = HashMap::new();

        for task in &self.config.tasks {
            let index = graph.add_node(task);
            task_to_node.insert(task as *const Task, index);
        }

        for left in &self.config.tasks {
            for right in &self.config.tasks {
                if right.depends_on(left)? {
                    let left_index = task_to_node[&(left as *const Task)];
                    let right_index = task_to_node[&(right as *const Task)];
                    // add edge: left ---> right
                    graph.update_edge(left_index, right_index, ());
                }
            }
        }

        debug!(
            "{node_count} tasks with {edge_count} edges",
            node_count = graph.node_count(),
            edge_count = graph.edge_count()
        );
        Ok(graph)
    }

    fn execute_graph(&self, graph: DiGraph<&Task, ()>, ctx: &impl Context) -> Result<RunResult> {
        let mut visitor = petgraph::visit::Topo::new(&graph);

        let mut task_results = vec![];

        while let Some(item) = visitor.next(&graph) {
            let task = graph[item];
            debug!("run task {task:?}", task = task);

            let result = task.execute(ctx).expect("task börked");

            task_results.push(TaskAndResult {
                task: task.clone(),
                result,
            });
        }

        Ok(RunResult { task_results })
    }
}

impl Run for GraphRun {
    fn new(config: Config) -> GraphRun {
        GraphRun { config }
    }
    fn run(&self, ctx: &impl Context) -> Result<RunResult> {
        let graph = self.get_graph()?;
        self.execute_graph(graph, ctx)
    }
}
