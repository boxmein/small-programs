use petgraph::graph::DiGraph;
use std::collections::HashMap;

use crate::{
    model::{Config, Task},
    traits::{Context, Executable},
};

pub fn execute_tasks<'a>(conf: &'a Config, ctx: &impl Context) {
    let mut graph = DiGraph::new();

    let mut task_to_node = HashMap::new();

    for task in &conf.tasks {
        let index = graph.add_node(task);
        task_to_node.insert(task as *const Task, index);
    }

    for left in &conf.tasks {
        for right in &conf.tasks {
            if right.depends_on(left) {
                let left_index = task_to_node[&(left as *const Task)];
                let right_index = task_to_node[&(right as *const Task)];
                // add edge: left ---> right
                graph.update_edge(left_index, right_index, ());
            }
        }
    }

    println!(
        "{} tasks with {} graph edges",
        graph.node_count(),
        graph.edge_count()
    );

    println!("{:?}", graph);

    let mut visitor = petgraph::visit::Topo::new(&graph);

    while let Some(item) = visitor.next(&graph) {
        let task = graph[item];
        println!("run task {:?}", task);

        task.execute(ctx).expect("task börked");
    }
}
