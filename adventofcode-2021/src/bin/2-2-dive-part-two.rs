//! --- Part Two ---

// Based on your calculations, the planned course doesn't seem to make any sense. You find the submarine manual and discover that the process is actually slightly more complicated.

// In addition to horizontal position and depth, you'll also need to track a third value, aim, which also starts at 0. The commands also mean something entirely different than you first thought:

// down X increases your aim by X units.
// up X decreases your aim by X units.
// forward X does two things:
// It increases your horizontal position by X units.
// It increases your depth by your aim multiplied by X.
// Again note that since you're on a submarine, down and up do the opposite of what you might expect: "down" means aiming in the positive direction.

// Now, the above example does something different:

// forward 5 adds 5 to your horizontal position, a total of 5. Because your aim is 0, your depth does not change.
// down 5 adds 5 to your aim, resulting in a value of 5.
// forward 8 adds 8 to your horizontal position, a total of 13. Because your aim is 5, your depth increases by 8*5=40.
// up 3 decreases your aim by 3, resulting in a value of 2.
// down 8 adds 8 to your aim, resulting in a value of 10.
// forward 2 adds 2 to your horizontal position, a total of 15. Because your aim is 10, your depth increases by 2*10=20 to a total of 60.
// After following these new instructions, you would have a horizontal position of 15 and a depth of 60. (Multiplying these produces 900.)

// Using this new interpretation of the commands, calculate the horizontal position and depth you would have after following the planned course. What do you get if you multiply your final horizontal position by your final depth?
use std::default::Default;
use std::io::{stdin, BufRead};
use std::str::FromStr;

// Submarine implementation

#[derive(Default)]
struct Submarine {
    pub x: i64,
    pub depth: i64,
    pub aim: i64,
}

impl Submarine {
    pub fn forward(&mut self, count: i64) {
        self.x += count;
        self.depth += self.aim * count;
    }
    pub fn down(&mut self, count: i64) {
        self.aim += count;
    }
    pub fn up(&mut self, count: i64) {
        self.aim -= count;
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
    Up(i64),
    Down(i64),
    Forward(i64),
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
        let coords: Vec<&str> = s.split(' ').collect();

        let word = coords[0];

        match word {
            "forward" => {
                let distance = coords[1]
                    .parse::<i64>()
                    .map_err(|_| "Failed to parse i32")?;
                Ok(Command::Forward(distance))
            }
            "down" => {
                let distance = coords[1]
                    .parse::<i64>()
                    .map_err(|_| "Failed to parse int32")?;
                Ok(Command::Down(distance))
            }
            "up" => {
                let distance = coords[1]
                    .parse::<i64>()
                    .map_err(|_| "Failed to parse int32")?;
                Ok(Command::Up(distance))
            }
            _ => Err(CommandParsingError("No such command")),
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

    for command in commands(stdin().lock()) {
        sub.accept(command);
    }

    let pos = sub.x;
    let depth = sub.depth;
    let aim = sub.aim;

    println!("Position: {}", pos);
    println!("Depth: {}", depth);
    println!("Aim: {}", aim);
    println!("Multiplied: {}", pos * depth);
}
