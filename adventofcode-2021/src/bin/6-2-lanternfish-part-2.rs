// --- Part Two ---

// Suppose the lanternfish live forever and have unlimited food and space. Would they take over the entire ocean?

// After 256 days in the example above, there would be a total of 26984457539 lanternfish!

// How many lanternfish would there be after 256 days?

use std::collections::HashMap;
use std::io::{stdin, BufRead};

const DAYS: usize = 256;

#[derive(Debug)]
struct Ocean(HashMap<u8, u64>);

impl Ocean {
    pub fn new(fishies: &[u8]) -> Self {
        let mut lanternfish_by_age: HashMap<u8, u64> = HashMap::new();

        for age in 0..9u8 {
            lanternfish_by_age.insert(age, 0);
        }

        for fish in fishies {
            lanternfish_by_age.insert(*fish, lanternfish_by_age[fish] + 1);
        }

        println!(
            "Added {} fishies to the ocean: {:?}",
            fishies.len(),
            lanternfish_by_age
        );

        Ocean(lanternfish_by_age)
    }

    pub fn count_fish_with_age(&self, age: u8) -> u64 {
        assert!(self.0.contains_key(&age));
        self.0[&age]
    }

    pub fn set_fish_with_age(&mut self, age: u8, count: u64) {
        assert!(age <= 8);
        self.0.insert(age, count);
    }

    pub fn add_fishes_to_age(&mut self, age: u8, count: u64) {
        self.set_fish_with_age(age, self.count_fish_with_age(age) + count);
    }

    pub fn multiply(&mut self, spawned_count: u64) {
        self.set_fish_with_age(8u8, spawned_count);
        self.add_fishes_to_age(6u8, spawned_count);
    }

    pub fn tick_day(&mut self) {
        let spawned_count = self.count_fish_with_age(0u8);

        for i in 0..8 {
            self.set_fish_with_age(i, self.count_fish_with_age(i + 1));
        }

        self.multiply(spawned_count);
    }

    pub fn total_fish(&mut self) -> u64 {
        let mut sum = 0;

        for i in 0..9u8 {
            sum += self.count_fish_with_age(i);
        }

        sum
    }
}

fn commands(stream: impl BufRead) -> impl Iterator<Item = String> {
    stream
        .lines()
        .filter(|value| value.is_ok())
        .map(|value| value.unwrap())
        .map(|value| value)
}

fn main() {
    let lanternfish = commands(stdin().lock())
        .next()
        .expect("expected one line of input")
        .split(",")
        .map(|item| item.parse::<u8>().expect("not uint8"))
        .collect::<Vec<u8>>();

    let mut ocean = Ocean::new(&lanternfish);
    println!("initial ocean: {:?}", ocean);

    for day in 0..DAYS {
        ocean.tick_day();

        println!(
            "day {:>3}: {:>9} lanternfish, {:>6} spawned ({:?})",
            day,
            ocean.total_fish(),
            ocean.count_fish_with_age(8u8),
            ocean
        );
    }
}
