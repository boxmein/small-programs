//! Day 2: Dive!
// Now, you need to figure out how to pilot this thing.
// It seems like the submarine can take a series of commands like forward 1, down 2, or up 3:
// forward X increases the horizontal position by X units.
// down X increases the depth by X units.
// up X decreases the depth by X units.
// Note that since you're on a submarine, down and up affect your depth, and so they have the opposite result of what you might expect.
// The submarine seems to already have a planned course (your puzzle input). You should probably figure out where it's going. For example:
// forward 5
// down 5
// forward 8
// up 3
// down 8
// forward 2
// Your horizontal position and depth both start at 0. The steps above would then modify them as follows:
// forward 5 adds 5 to your horizontal position, a total of 5.
// down 5 adds 5 to your depth, resulting in a value of 5.
// forward 8 adds 8 to your horizontal position, a total of 13.
// up 3 decreases your depth by 3, resulting in a value of 2.
// down 8 adds 8 to your depth, resulting in a value of 10.
// forward 2 adds 2 to your horizontal position, a total of 15.
// After following these instructions, you would have a horizontal position of 15 and a depth of 10. (Multiplying these together produces 150.)
// Calculate the horizontal position and depth you would have after following the planned course. What do you get if you multiply your final horizontal position by your final depth?

use std::io::{stdin, BufRead};
use std::default::Default;
use std::str::FromStr;

// Submarine implementation

#[derive(Default)]
struct Submarine {
    pub x: i32,
    pub depth: i32,
}

impl Submarine {
    pub fn forward(&mut self, count: i32) { 
        self.x += count;
    }
    pub fn down(&mut self, count: i32) {
        self.depth += count;
    }
    pub fn up(&mut self, count: i32) {
        self.depth -= count;
    }

    pub fn accept(&mut self, command: Command) {
        match command {
            Command::Up(count) => self.up(count),
            Command::Down(count) => self.down(count),
            Command::Forward(count) => self.forward(count),
        }
    }
}

// Command implementation

#[derive(Debug)]
enum Command {
    Up(i32),
    Down(i32),
    Forward(i32)
}

#[derive(Debug)]
struct CommandParsingError(&'static str);

impl From<&'static str> for CommandParsingError {
    fn from(s: &'static str) -> Self {
        CommandParsingError(s)
    }
}

impl FromStr for Command {
    type Err = CommandParsingError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let coords: Vec<&str> = s
                                 .split(' ')
                                 .collect();

        let word = coords[0];

        match word {
            "forward" => {
                let distance = coords[1].parse::<i32>().map_err(|_| "Failed to parse i32")?;
                Ok(
                    Command::Forward(distance)
                )
            },
            "down" => {
                let distance = coords[1].parse::<i32>().map_err(|_| "Failed to parse int32")?;
                Ok(
                    Command::Down(distance)
                )
            },
            "up" => {
                let distance = coords[1].parse::<i32>().map_err(|_| "Failed to parse int32")?;
                Ok(
                    Command::Up(distance)
                )
            }
            _ => Err(CommandParsingError("No such command"))
        }
    }
}

fn commands(stream: impl BufRead) -> impl Iterator<Item = Command> {
    stream
        .lines()
        .filter(|value| value.is_ok())
        .map(|value| value.unwrap())
        .map(|value| value.parse::<Command>().expect("invalid command"))
}

/// Usage:
/// ./2-dive < inputtext
fn main() {
    let mut sub = Submarine::default();

    for command in commands(stdin()
    .lock()) {
        sub.accept(command);
    }

    let pos = sub.x;
    let depth = sub.depth;

    println!("Position: {}", pos);
    println!("Depth: {}", depth);
    println!("Multiplied: {}", pos * depth);
}