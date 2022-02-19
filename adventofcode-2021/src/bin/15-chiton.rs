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

// --- Part Two ---

// Now that you know how to find low-risk paths in the cave, you can try to find your way out.

// The entire cave is actually five times larger in both dimensions than you thought; the area you originally scanned is just one tile in a 5x5 tile area that forms the full map. Your original map tile repeats to the right and downward; each time the tile repeats to the right or downward, all of its risk levels are 1 higher than the tile immediately up or left of it. However, risk levels above 9 wrap back around to 1. So, if your original map had some position with a risk level of 8, then that same position on each of the 25 total tiles would be as follows:

// 8 9 1 2 3
// 9 1 2 3 4
// 1 2 3 4 5
// 2 3 4 5 6
// 3 4 5 6 7

// Each single digit above corresponds to the example position with a value of 8 on the top-left tile. Because the full map is actually five times larger in both dimensions, that position appears a total of 25 times, once in each duplicated tile, with the values shown above.

// Here is the full five-times-as-large version of the first example above, with the original map in the top left corner highlighted:

// 11637517422274862853338597396444961841755517295286
// 13813736722492484783351359589446246169155735727126
// 21365113283247622439435873354154698446526571955763
// 36949315694715142671582625378269373648937148475914
// 74634171118574528222968563933317967414442817852555
// 13191281372421239248353234135946434524615754563572
// 13599124212461123532357223464346833457545794456865
// 31254216394236532741534764385264587549637569865174
// 12931385212314249632342535174345364628545647573965
// 23119445813422155692453326671356443778246755488935
// 22748628533385973964449618417555172952866628316397
// 24924847833513595894462461691557357271266846838237
// 32476224394358733541546984465265719557637682166874
// 47151426715826253782693736489371484759148259586125
// 85745282229685639333179674144428178525553928963666
// 24212392483532341359464345246157545635726865674683
// 24611235323572234643468334575457944568656815567976
// 42365327415347643852645875496375698651748671976285
// 23142496323425351743453646285456475739656758684176
// 34221556924533266713564437782467554889357866599146
// 33859739644496184175551729528666283163977739427418
// 35135958944624616915573572712668468382377957949348
// 43587335415469844652657195576376821668748793277985
// 58262537826937364893714847591482595861259361697236
// 96856393331796741444281785255539289636664139174777
// 35323413594643452461575456357268656746837976785794
// 35722346434683345754579445686568155679767926678187
// 53476438526458754963756986517486719762859782187396
// 34253517434536462854564757396567586841767869795287
// 45332667135644377824675548893578665991468977611257
// 44961841755517295286662831639777394274188841538529
// 46246169155735727126684683823779579493488168151459
// 54698446526571955763768216687487932779859814388196
// 69373648937148475914825958612593616972361472718347
// 17967414442817852555392896366641391747775241285888
// 46434524615754563572686567468379767857948187896815
// 46833457545794456865681556797679266781878137789298
// 64587549637569865174867197628597821873961893298417
// 45364628545647573965675868417678697952878971816398
// 56443778246755488935786659914689776112579188722368
// 55172952866628316397773942741888415385299952649631
// 57357271266846838237795794934881681514599279262561
// 65719557637682166874879327798598143881961925499217
// 71484759148259586125936169723614727183472583829458
// 28178525553928963666413917477752412858886352396999
// 57545635726865674683797678579481878968159298917926
// 57944568656815567976792667818781377892989248891319
// 75698651748671976285978218739618932984172914319528
// 56475739656758684176786979528789718163989182927419
// 67554889357866599146897761125791887223681299833479

// Equipped with the full map, you can now find a path from the top left corner to the bottom right corner with the lowest total risk:

// 11637517422274862853338597396444961841755517295286
// 13813736722492484783351359589446246169155735727126
// 21365113283247622439435873354154698446526571955763
// 36949315694715142671582625378269373648937148475914
// 74634171118574528222968563933317967414442817852555
// 13191281372421239248353234135946434524615754563572
// 13599124212461123532357223464346833457545794456865
// 31254216394236532741534764385264587549637569865174
// 12931385212314249632342535174345364628545647573965
// 23119445813422155692453326671356443778246755488935
// 22748628533385973964449618417555172952866628316397
// 24924847833513595894462461691557357271266846838237
// 32476224394358733541546984465265719557637682166874
// 47151426715826253782693736489371484759148259586125
// 85745282229685639333179674144428178525553928963666
// 24212392483532341359464345246157545635726865674683
// 24611235323572234643468334575457944568656815567976
// 42365327415347643852645875496375698651748671976285
// 23142496323425351743453646285456475739656758684176
// 34221556924533266713564437782467554889357866599146
// 33859739644496184175551729528666283163977739427418
// 35135958944624616915573572712668468382377957949348
// 43587335415469844652657195576376821668748793277985
// 58262537826937364893714847591482595861259361697236
// 96856393331796741444281785255539289636664139174777
// 35323413594643452461575456357268656746837976785794
// 35722346434683345754579445686568155679767926678187
// 53476438526458754963756986517486719762859782187396
// 34253517434536462854564757396567586841767869795287
// 45332667135644377824675548893578665991468977611257
// 44961841755517295286662831639777394274188841538529
// 46246169155735727126684683823779579493488168151459
// 54698446526571955763768216687487932779859814388196
// 69373648937148475914825958612593616972361472718347
// 17967414442817852555392896366641391747775241285888
// 46434524615754563572686567468379767857948187896815
// 46833457545794456865681556797679266781878137789298
// 64587549637569865174867197628597821873961893298417
// 45364628545647573965675868417678697952878971816398
// 56443778246755488935786659914689776112579188722368
// 55172952866628316397773942741888415385299952649631
// 57357271266846838237795794934881681514599279262561
// 65719557637682166874879327798598143881961925499217
// 71484759148259586125936169723614727183472583829458
// 28178525553928963666413917477752412858886352396999
// 57545635726865674683797678579481878968159298917926
// 57944568656815567976792667818781377892989248891319
// 75698651748671976285978218739618932984172914319528
// 56475739656758684176786979528789718163989182927419
// 67554889357866599146897761125791887223681299833479

// The total risk of this path is 315 (the starting position is still never entered, so its risk is not counted).

// Using the full map, what is the lowest total risk of any path from the top left to the bottom right?

use ndarray::prelude::*;
use petgraph::algo::astar;
use petgraph::graph::{DiGraph, NodeIndex};
use std::io::{stdin, BufRead};
use std::str::FromStr;

type EdgeData = u64;
type NodeData = u64;

#[derive(Default, Debug)]
struct Floor {
    pub locations: DiGraph<NodeData, EdgeData>,
    pub start: NodeIndex,
    pub end: NodeIndex,
}

fn digit_grid_to_ndarray(s: &str) -> Array2<u64> {
    let arr = s
        .split("\n")
        .filter(|x| x.len() > 0)
        .map(|x| {
            x.chars()
                .map(|col| col.to_digit(10).unwrap().try_into().unwrap())
                .collect::<Vec<NodeData>>()
        })
        .collect::<Vec<Vec<NodeData>>>();

    let height = arr.len();
    let width = arr[0].len();

    let arr = arr.into_iter().flatten().collect::<Vec<NodeData>>();

    Array2::from_shape_vec((height, width), arr).expect("failed to create array")
}

fn copy_into(av: &mut ArrayViewMut2<u64>, data: &Array2<u64>) {
    assert_eq!(
        av.shape(),
        data.shape(),
        "copy_into: array view shape {:?} does not equal data shape {:?}",
        av.shape(),
        data.shape()
    );

    for y in 0..av.shape()[0] {
        for x in 0..av.shape()[1] {
            av[[y, x]] = data[[y, x]];
        }
    }
}

fn repeat_ndarray(n: &Array2<u64>) -> Array2<u64> {
    let new_shape = n.shape().iter().map(|i| i * 5).collect::<Vec<usize>>();

    let height = n.shape()[0];
    let width = n.shape()[1];

    let mut shp: [usize; 2] = Default::default();
    shp.copy_from_slice(&new_shape);

    let mut arr = Array2::from_shape_fn(shp, |_| 0);

    // Copies work like this:
    // A is the original dataset,
    // B is A where every item has 1 added to it,
    // C is B where every item has 1 added to it,
    // and so on.
    // All items are in modulo 9 arithmetic + 1 (123456789123456789...)
    // A B C D E
    // B C D E F
    // C D E F G
    // D E F G H
    // E F G H I

    for y in 0..5 {
        for x in 0..5 {
            // find the slice's positions:
            // +------+ 12345
            // | 1234 | 04291
            // +------+ 04219
            // 40285473824290
            let left_x = x * width;
            let right_x = (x + 1) * width - 1;
            let top_y = y * height;
            let bottom_y = (y + 1) * height - 1;

            // find the amount we need to add to the original block, to make it
            // correct
            let delta = (x + y) as u64;

            // create the data block to copy from
            let data_block = n + delta;

            // perform copy from data block to mega ndarray
            copy_into(
                &mut arr.slice_mut(s![top_y..bottom_y + 1, left_x..right_x + 1]),
                &data_block,
            );
        }
    }

    arr
}

fn digit_array_to_graph(
    inp: &Array2<u64>,
) -> (
    DiGraph<NodeData, EdgeData>,
    NodeIndex, // start of traversal
    NodeIndex, // end of traversal
) {
    macro_rules! access_node {
        ($n:expr, $x:expr, $y:expr) => {
            $n.get($y).map(|row| row.get($x)).flatten()
        };
    }

    let mut g: DiGraph<NodeData, EdgeData> = DiGraph::new();

    let nodes: Vec<Vec<NodeIndex>> = inp
        .outer_iter()
        .map(|row| {
            row.outer_iter()
                .map(|col| g.add_node(*col.into_scalar()))
                .collect::<Vec<NodeIndex>>()
        })
        .collect();

    // Add edges to graph
    for (y, row) in nodes.iter().enumerate() {
        for (x, _) in row.iter().enumerate() {
            let center = nodes[y][x];

            if x > 0 {
                if let Some(left) = access_node!(nodes, x - 1, y) {
                    g.add_edge(center, *left, g[*left]);
                }
            }

            if let Some(right) = access_node!(nodes, x + 1, y) {
                g.add_edge(center, *right, g[*right]);
            }
            if y > 0 {
                if let Some(up) = access_node!(nodes, x, y - 1) {
                    g.add_edge(center, *up, g[*up]);
                }
            }
            if let Some(down) = access_node!(nodes, x, y + 1) {
                g.add_edge(center, *down, g[*down]);
            }
        }
    }

    let max_x = inp.shape()[1] - 1;
    let max_y = inp.shape()[0] - 1;

    let start = *access_node!(nodes, 0, 0).unwrap();
    let end = *access_node!(nodes, max_x, max_y).unwrap();

    (g, start, end)
}

impl FromStr for Floor {
    type Err = &'static str;
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let mut f = Floor::default();

        let digits = digit_grid_to_ndarray(value);

        let expanded_digits = repeat_ndarray(&digits);

        let (graph, start, end) = digit_array_to_graph(&expanded_digits);

        f.locations = graph;
        f.start = start;
        f.end = end;

        Ok(f)
    }
}

fn correct_weight(weight: u64) -> u64 {
    if weight > 9 {
        (weight - 1) % 9 + 1
    } else if weight < 1 {
        1
    } else {
        weight
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

    if let Some((k, path)) = astar(
        &floor.locations,
        floor.start,
        |n| n == floor.end,
        |e| correct_weight(*e.weight()),
        |_| 0,
    ) {
        println!("lowest risk score: {}", k);
    }
}

#[cfg(test)]
mod tests {
    use ndarray::prelude::*;

    use super::*;
    use petgraph::graph::EdgeIndex;
    #[test]
    fn floor_parses() {
        let s = r#"123
232
321"#;

        let floor = s.parse::<Floor>().unwrap();

        assert_eq!(floor.locations.node_count(), 225);
        assert_eq!(floor.locations.edge_count(), 840);

        // assuming node 0 = (0, 0); node 1 = (1, 0); node 2 = (2, 0)
        assert_eq!(floor.locations[NodeIndex::new(0)], 1);
        assert_eq!(floor.locations[NodeIndex::new(1)], 2);
        assert_eq!(floor.locations[NodeIndex::new(2)], 3);

        // assuming edge 0 = ((0, 0) -> (1, 0))
        assert_eq!(floor.locations[EdgeIndex::new(0)], 2);
    }

    // NOTE: in order to test this, we would have to make the repeat_ndarray
    // step conditional. Not right now.
    #[test]
    #[ignore]
    fn example_works() {
        let s = r#"1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"#;

        let arr = digit_grid_to_ndarray(s);
        let (g, start, end) = digit_array_to_graph(&arr);
        if let Some((k, path)) = astar(&g, start, |n| n == end, |e| *e.weight(), |_| 0) {
            assert_eq!(k, 40);
        } else {
            panic!("received None from a*");
        }
    }

    #[test]
    fn digit_grid_to_ndarray_works() {
        let s = r#"123
232
321"#;
        let arr = digit_grid_to_ndarray(s);

        assert_eq!(arr[[0, 0]], 1);
        assert_eq!(arr[[0, 1]], 2);
        assert_eq!(arr[[0, 2]], 3);
        assert_eq!(arr[[1, 0]], 2);
        assert_eq!(arr[[1, 1]], 3);
        assert_eq!(arr[[1, 2]], 2);
        assert_eq!(arr[[2, 0]], 3);
        assert_eq!(arr[[2, 1]], 2);
        assert_eq!(arr[[2, 2]], 1);
    }

    #[test]
    fn digit_array_to_graph_works() {
        let arr = arr2(&[[1u64, 2, 3], [2, 3, 2], [3, 2, 1]]);

        let (g, start, end) = digit_array_to_graph(&arr);

        assert_eq!(g.node_count(), 9);
        assert_eq!(g.edge_count(), 24);
        g[start];
        g[end];
    }

    #[test]
    fn repeat_ndarray_increases_size() {
        let arr = arr2(&[[1u64, 2, 3], [2, 3, 2], [3, 2, 1]]);

        let shape = (arr.shape()[0], arr.shape()[1]);

        let arr = repeat_ndarray(&arr);

        let shape2 = arr.shape();

        assert_eq!(shape2, &[15, 15]);
    }

    #[test]
    fn repeat_ndarray_has_original_contents() {
        let arr = arr2(&[[1u64, 2, 3], [2, 3, 2], [3, 2, 1]]);

        let arr = repeat_ndarray(&arr);

        assert_eq!(
            arr.slice(s![0..3, 0..3]),
            arr2(&[[1, 2, 3], [2, 3, 2], [3, 2, 1]])
        );
    }

    #[test]
    fn repeat_ndarray_has_correct_repeat_at_1_0() {
        let arr = arr2(&[[1u64, 2, 3], [2, 3, 2], [3, 2, 1]]);

        let arr = repeat_ndarray(&arr);

        assert_eq!(
            arr.slice(s![3..6, 0..3]),
            arr2(&[[2, 3, 4], [3, 4, 3], [4, 3, 2]])
        );
    }

    #[test]
    fn repeat_ndarray_has_correct_repeat_at_2_0() {
        let arr = arr2(&[[1u64, 2, 3], [2, 3, 2], [3, 2, 1]]);

        let arr = repeat_ndarray(&arr);

        assert_eq!(
            arr.slice(s![6..9, 0..3]),
            arr2(&[[3, 4, 5], [4, 5, 4], [5, 4, 3]])
        );
    }

    #[test]
    fn repeat_ndarray_has_correct_repeat_at_4_0() {
        let arr = arr2(&[[1u64, 2, 3], [2, 3, 2], [3, 2, 1]]);

        let arr = repeat_ndarray(&arr);

        assert_eq!(
            arr.slice(s![12..15, 0..3]),
            arr2(&[[5, 6, 7], [6, 7, 6], [7, 6, 5]])
        );
    }

    #[test]
    fn repeat_ndarray_has_correct_repeat_at_4_4() {
        let arr = arr2(&[[1u64, 2, 3], [2, 3, 2], [3, 2, 1]]);

        let arr = repeat_ndarray(&arr);

        assert_eq!(
            arr.slice(s![12..15, 12..15]),
            arr2(&[[9, 10, 11], [10, 11, 10], [11, 10, 9]])
        );
    }

    #[test]
    fn copy_into_works() {
        let mut ar1 = arr2(&[[0, 0, 1], [0, 0, 2], [5, 4, 3]]);

        let ar2 = arr2(&[[6, 7], [8, 9]]);

        copy_into(&mut ar1.slice_mut(s![0..2, 0..2]), &ar2);

        assert_eq!(ar1, arr2(&[[6, 7, 1], [8, 9, 2], [5, 4, 3],]));
    }

    #[test]
    fn weight_corrector_works() {
        assert_eq!(correct_weight(1), 1);
        assert_eq!(correct_weight(2), 2);
        assert_eq!(correct_weight(3), 3);
        assert_eq!(correct_weight(4), 4);
        assert_eq!(correct_weight(5), 5);
        assert_eq!(correct_weight(6), 6);
        assert_eq!(correct_weight(7), 7);
        assert_eq!(correct_weight(8), 8);
        assert_eq!(correct_weight(9), 9);
        assert_eq!(correct_weight(10), 1);
        assert_eq!(correct_weight(11), 2);
        assert_eq!(correct_weight(12), 3);
        assert_eq!(correct_weight(13), 4);
        assert_eq!(correct_weight(14), 5);
        assert_eq!(correct_weight(15), 6);
        assert_eq!(correct_weight(16), 7);
        assert_eq!(correct_weight(17), 8);
        assert_eq!(correct_weight(18), 9);
        assert_eq!(correct_weight(19), 1);
        assert_eq!(correct_weight(20), 2);
        assert_eq!(correct_weight(21), 3);
    }
}
