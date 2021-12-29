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
use petgraph::graph::{NodeIndex, EdgeIndex, DiGraph};
use petgraph::data::FromElements;
use petgraph::visit::NodeRef;
use petgraph::algo::astar;
use std::cmp::max;

macro_rules! access_node {
    ($n:expr, $x:expr, $y:expr) => {
        $n.get($y).map(|row| row.get($x)).flatten()
    }
}


type NodeData = u64;
type EdgeData = u64;



#[derive(Default, Debug)]
struct Floor {
    pub locations: DiGraph<NodeData, EdgeData>,
    pub start: NodeIndex,
    pub end: NodeIndex,
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
                    let weight: NodeData = col
                        .to_digit(10)
                        .unwrap()
                        .try_into()
                        .unwrap();
                    f.locations.add_node(weight)
                }).collect::<Vec<NodeIndex>>()
            }).collect();

        let mut max_x = 0;
        let mut max_y = 0;

        // Add edges to graph
        for (y, row) in nodes.iter().enumerate() {
            if y > max_y {
                max_y = y;
            }
            for (x, _) in row.iter().enumerate() {
                if x > max_x {
                    max_x = x;
                }
                let center = nodes[y][x];
                
                if x > 0 {
                    if let Some(left) = access_node!(nodes, x-1, y) {
                        f.locations.add_edge(
                            center,
                            *left,
                            f.locations[*left],
                        );
                    }
                }

                if let Some(right) = access_node!(nodes, x+1, y) {
                    f.locations.add_edge(
                        center,
                        *right,
                        f.locations[*right],
                    );
                }
                if y > 0 {
                    if let Some(up) = access_node!(nodes, x, y-1) {
                        f.locations.add_edge(
                            center,
                            *up,
                            f.locations[*up],
                        );
                    }
                }
                if let Some(down) = access_node!(nodes, x, y+1) {
                    f.locations.add_edge(
                        center,
                        *down,
                        f.locations[*down],
                    );
                }
            }
        }

        f.start = *access_node!(nodes, 0, 0).unwrap();
        f.end = *access_node!(nodes, max_x, max_y).unwrap();

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

    // println!("{:?}", floor);

    if let Some((k, path)) = astar(
        &floor.locations,
        floor.start,
        |n| n == floor.end,
        |e| *e.weight(),
        |_| 0
    ) {
        println!("lowest risk score: {}", k);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn floor_parses() {
        let s = r#"123
232
321"#;

        let floor = s.parse::<Floor>().unwrap();

        assert_eq!(floor.locations.node_count(), 9);
        assert_eq!(floor.locations.edge_count(), 24);

        // assuming node 0 = (0, 0); node 1 = (1, 0); node 2 = (2, 0)
        assert_eq!(floor.locations[NodeIndex::new(0)], 1);
        assert_eq!(floor.locations[NodeIndex::new(1)], 2);
        assert_eq!(floor.locations[NodeIndex::new(2)], 3);

        // assuming edge 0 = ((0, 0) -> (1, 0))
        assert_eq!(floor.locations[EdgeIndex::new(0)], 2);
    }
}