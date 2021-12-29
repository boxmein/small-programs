//! --- Day 15: Chiton ---
// You've almost reached the exit of the cave, but the walls are getting closer together. Your submarine can barely still fit, though; the main problem is that the walls of the cave are covered in chitons, and it would be best not to bump any of them.

// The cavern is large, but has a very low ceiling, restricting your motion to two dimensions. The shape of the cavern resembles a square; a quick scan of chiton density produces a map of risk level throughout the cave (your puzzle input). For example:

// 1163751742
// 1381373672
// 2136511328
// 3694931569
// 7463417111
// 1319128137
// 1359912421
// 3125421639
// 1293138521
// 2311944581

// You start in the top left position, your destination is the bottom right position, and you cannot move diagonally. The number at each position is its risk level; to determine the total risk of an entire path, add up the risk levels of each position you enter (that is, don't count the risk level of your starting position unless you enter it; leaving it adds no risk to your total).

// Your goal is to find a path with the lowest total risk. In this example, a path with the lowest total risk is highlighted here:

// 1163751742
// 1381373672
// 2136511328
// 3694931569
// 7463417111
// 1319128137
// 1359912421
// 3125421639
// 1293138521
// 2311944581

// The total risk of this path is 40 (the starting position is never entered, so its risk is not counted).

// What is the lowest total risk of any path from the top left to the bottom right?

// To begin, get your puzzle input.

use std::io::{stdin, BufRead};
use std::collections::HashMap;
use std::str::FromStr;
use petgraph::graph::{NodeIndex, UnGraph};
use petgraph::data::FromElements;
use std::cmp::max;

type NodeData = u8;
type EdgeData = u8;

#[derive(Default, Debug)]
struct Floor {
    pub locations: UnGraph<NodeData, EdgeData>,
}

impl FromStr for Floor {
    type Err = &'static str;
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let mut f = Floor::default();

        let arr = value
            .split("\n")
            .filter(|x| x.len() > 0)
            .map(|x| x.to_owned())
            .collect::<Vec<String>>();

        // Add vertices to graph
        let nodes: Vec<Vec<NodeIndex>> = 
            arr.iter().map(|row| {
                row.chars().map(|col| {
                    let weight: u8 = col
                        .to_digit(10)
                        .unwrap()
                        .try_into()
                        .unwrap();
                    f.locations.add_node(weight)
                }).collect::<Vec<NodeIndex>>()
            }).collect();

        // Add edges to graph
        for (y, row) in nodes.iter().enumerate() {
            for (x, _) in row.iter().enumerate() {
                let center = nodes[y][x];
                
                if x > 0 {
                    if let Some(left) = nodes.get(y).map(|row| row.get(x-1)).flatten() {
                        f.locations.add_edge(
                            center,
                            *left,
                            0,
                        );
                    }
                }

                if let Some(right) = nodes.get(y).map(|row| row.get(x+1)).flatten() {
                    f.locations.add_edge(
                        center,
                        *right,
                        0,
                    );
                }
                if y > 0 {
                    if let Some(up) = nodes.get(y - 1).map(|row| row.get(x)).flatten() {
                        f.locations.add_edge(
                            center,
                            *up,
                            0,
                        );
                    }
                }
                if let Some(down) = nodes.get(y + 1).map(|row| row.get(x)).flatten() {
                    f.locations.add_edge(
                        center,
                        *down,
                        0,
                    );
                }
            }
        }

        Ok(f)
    }
}




fn main() {
    let mut s: String = String::default();
    for line in stdin()
        .lock()
        .lines()
        .filter(|value| value.is_ok())
        .map(|value| value.expect("stdin failure"))
    {
        s += &line;
        s += "\n";
    }

    println!("loaded {} lines", s.split("\n").count());

    let floor = s.parse::<Floor>().expect("failed to parse floor");

    println!("{:?}", floor);

}
