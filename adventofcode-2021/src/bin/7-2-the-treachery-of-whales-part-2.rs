// --- Part Two ---

// The crabs don't seem interested in your proposed solution. Perhaps you misunderstand crab engineering?

// As it turns out, crab submarine engines don't burn fuel at a constant rate. Instead, each change of 1 step in horizontal position costs 1 more unit of fuel than the last: the first step costs 1, the second step costs 2, the third step costs 3, and so on.

// As each crab moves, moving further becomes more expensive. This changes the best horizontal position to align them all on; in the example above, this becomes 5:

// Move from 16 to 5: 66 fuel
// Move from 1 to 5: 10 fuel
// Move from 2 to 5: 6 fuel
// Move from 0 to 5: 15 fuel
// Move from 4 to 5: 1 fuel
// Move from 2 to 5: 6 fuel
// Move from 7 to 5: 3 fuel
// Move from 1 to 5: 10 fuel
// Move from 2 to 5: 6 fuel
// Move from 14 to 5: 45 fuel
// This costs a total of 168 fuel. This is the new cheapest possible outcome; the old alignment position (2) now costs 206 fuel instead.

// Determine the horizontal position that the crabs can align to using the least fuel possible so they can make you an escape route! How much fuel must they spend to align to that position?

use std::io::{stdin, BufRead};

fn commands(stream: impl BufRead) -> Vec<u64> {
    stream
        .lines()
        .filter(|value| value.is_ok())
        .map(|value| value.unwrap())
        .map(|value| value)
        .next()
        .expect("expected one line of input")
        .split(",")
        .map(|item| item.parse::<u64>().expect("not uint32"))
        .collect::<Vec<u64>>()
}

fn fuel_part_two(fuel: i64) -> i64 {
    // suboptimal: (1..(fuel + 1)).sum()

    // https://stackoverflow.com/a/30386168
    ((fuel - 1) + 1) * (1 + fuel) / 2
}

fn fuel(crabs: &[u64], to_pos: u64) -> i64 {
    let to_pos = to_pos as i64;

    crabs
        .iter()
        .map(|crab| (*crab as i64) - to_pos)
        .map(|fuel| fuel.abs())
        .map(|fuel| fuel_part_two(fuel))
        .sum()
}

fn main() {
    let crabs = commands(stdin().lock());
    let min = *crabs.iter().min().unwrap();
    let max = *crabs.iter().max().unwrap();

    let mut best = (0, std::i64::MAX);
    for i in min..(max + 1) {
        let f = fuel(&crabs, i);
        if f < best.1 {
            best = (i, f);
        }
    }

    println!("best: pos {}, dist {}", best.0, best.1);
}

#[cfg(test)]
mod tests {
    use super::{fuel, fuel_part_two};

    #[test]
    fn fuel_is_correct() {
        // example input
        assert_eq!(fuel(&[16, 1, 2, 0, 4, 2, 7, 1, 2, 14], 2), 206)
    }

    #[test]
    fn fuel_part_two_is_correct() {
        assert_eq!(fuel_part_two(4), 10);

        assert_eq!(fuel_part_two(16 - 5), 66);

        assert_eq!(fuel_part_two(5 - 2), 6);

        assert_eq!(fuel_part_two(5 - 0), 15);

        assert_eq!(fuel_part_two(14 - 5), 45);
    }
}
