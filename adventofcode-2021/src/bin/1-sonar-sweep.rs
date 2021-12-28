//! --- Day 1: Sonar Sweep ---
// You're minding your own business on a ship at sea when the overboard alarm goes off! You rush to see if you can help. Apparently, one of the Elves tripped and accidentally sent the sleigh keys flying into the ocean!
// Before you know it, you're inside a submarine the Elves keep ready for situations like this. It's covered in Christmas lights (because of course it is), and it even has an experimental antenna that should be able to track the keys if you can boost its signal strength high enough; there's a little meter that indicates the antenna's signal strength by displaying 0-50 stars.
// Your instincts tell you that in order to save Christmas, you'll need to get all fifty stars by December 25th.
// Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
// As the submarine drops below the surface of the ocean, it automatically performs a sonar sweep of the nearby sea floor. On a small screen, the sonar sweep report (your puzzle input) appears: each line is a measurement of the sea floor depth as the sweep looks further and further away from the submarine.
// For example, suppose you had the following report:
// 199
// 200
// 208
// 210
// 200
// 207
// 240
// 269
// 260
// 263
// This report indicates that, scanning outward from the submarine, the sonar sweep found depths of 199, 200, 208, 210, and so on.
// The first order of business is to figure out how quickly the depth increases, just so you know what you're dealing with - you never know if the keys will get carried into deeper water by an ocean current or a fish or something.
// To do this, count the number of times a depth measurement increases from the previous measurement. (There is no measurement before the first measurement.) In the example above, the changes are as follows:
// 199 (N/A - no previous measurement)
// 200 (increased)
// 208 (increased)
// 210 (increased)
// 200 (decreased)
// 207 (increased)
// 240 (increased)
// 269 (increased)
// 260 (decreased)
// 263 (increased)
// In this example, there are 7 measurements that are larger than the previous measurement.
// How many measurements are larger than the previous measurement?

use itertools::Itertools;
use std::io::{stdin, BufRead};

fn iterate_over_windows(reader: impl BufRead) -> impl Iterator<Item = (String, String)> {
    let lines = reader
        .lines()
        .filter(|value| value.is_ok())
        .map(|value| value.unwrap());
    return lines.tuple_windows();
}

/// Usage:
/// ./1-sonar-sweep < puzzleinput.txt
fn main() {
    // Implementation of the solution for the first half
    // of the puzzle.
    let mut increased_count = 0;
    for (prev, next) in iterate_over_windows(stdin().lock()) {
        if prev == "" || next == "" {
            return;
        }
        let prev = prev.parse::<i32>().expect("Failed to parse as int");
        let next = next.parse::<i32>().expect("Failed to parse as int");

        let action = if prev < next {
            "increased"
        } else if prev > next {
            "decreased"
        } else {
            "equal"
        };

        if action == "increased" {
            increased_count += 1;
        }

        println!("{} ({})", next, action);
    }
    println!("Result: {}", increased_count);
}
