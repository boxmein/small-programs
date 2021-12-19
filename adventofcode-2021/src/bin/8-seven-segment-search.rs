
// --- Day 8: Seven Segment Search ---
//region Intro
// You barely reach the safety of the cave when the whale smashes into the cave mouth, collapsing it. Sensors indicate another exit to this cave at a much greater depth, so you have no choice but to press on.

// As your submarine slowly makes its way through the cave system, you notice that the four-digit seven-segment displays in your submarine are malfunctioning; they must have been damaged during the escape. You'll be in a lot of trouble without them, so you'd better figure out what's wrong.

// Each digit of a seven-segment display is rendered by turning on or off any of seven segments named a through g:

//   0:      1:      2:      3:      4:
//  aaaa    ....    aaaa    aaaa    ....
// b    c  .    c  .    c  .    c  b    c
// b    c  .    c  .    c  .    c  b    c
//  ....    ....    dddd    dddd    dddd
// e    f  .    f  e    .  .    f  .    f
// e    f  .    f  e    .  .    f  .    f
//  gggg    ....    gggg    gggg    ....

//   5:      6:      7:      8:      9:
//  aaaa    aaaa    aaaa    aaaa    aaaa
// b    .  b    .  .    c  b    c  b    c
// b    .  b    .  .    c  b    c  b    c
//  dddd    dddd    ....    dddd    dddd
// .    f  e    f  .    f  e    f  .    f
// .    f  e    f  .    f  e    f  .    f
//  gggg    gggg    ....    gggg    gggg
// So, to render a 1, only segments c and f would be turned on; the rest would be off. To render a 7, only segments a, c, and f would be turned on.

// The problem is that the signals which control the segments have been mixed up on each display. The submarine is still trying to display numbers by producing output on signal wires a through g, but those wires are connected to segments randomly. Worse, the wire/segment connections are mixed up separately for each four-digit display! (All of the digits within a display use the same connections, though.)

// So, you might know that only signal wires b and g are turned on, but that doesn't mean segments b and g are turned on: the only digit that uses two segments is 1, so it must mean segments c and f are meant to be on. With just that information, you still can't tell which wire (b/g) goes to which segment (c/f). For that, you'll need to collect more information.

// For each display, you watch the changing signals for a while, make a note of all ten unique signal patterns you see, and then write down a single four digit output value (your puzzle input). Using the signal patterns, you should be able to work out which pattern corresponds to which digit.

// For example, here is what you might see in a single entry in your notes:

// acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
// cdfeb fcadb cdfeb cdbaf
// (The entry is wrapped here to two lines so it fits; in your notes, it will all be on a single line.)

// Each entry consists of ten unique signal patterns, a | delimiter, and finally the four digit output value. Within an entry, the same wire/segment connections are used (but you don't know what the connections actually are). The unique signal patterns correspond to the ten different ways the submarine tries to render a digit using the current wire/segment connections. Because 7 is the only digit that uses three segments, dab in the above example means that to render a 7, signal lines d, a, and b are on. Because 4 is the only digit that uses four segments, eafb means that to render a 4, signal lines e, a, f, and b are on.

// Using this information, you should be able to work out which combination of signal wires corresponds to each of the ten digits. Then, you can decode the four digit output value. Unfortunately, in the above example, all of the digits in the output value (cdfeb fcadb cdfeb cdbaf) use five segments and are more difficult to deduce.

// For now, focus on the easy digits. Consider this larger example:

// be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
// fdgacbe cefdb cefbgd gcbe
// edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |
// fcgedb cgb dgebacf gc
// fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |
// cg cg fdcagb cbg
// fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |
// efabcd cedba gadfec cb
// aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |
// gecf egdcabf bgf bfgea
// fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |
// gebdcfa ecba ca fadegcb
// dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |
// cefg dcbef fcge gbcadfe
// bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |
// ed bcgafe cdgba cbgef
// egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |
// gbdfcae bgc cg cgb
// gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |
// fgae cfgab fg bagce
// Because the digits 1, 4, 7, and 8 each use a unique number of segments, you should be able to tell which combinations of signals correspond to those digits. Counting only digits in the output values (the part after | on each line), in the above example, there are 26 instances of digits that use a unique number of segments (highlighted above).
//endregion

// In the output values, how many times do digits 1, 4, 7, or 8 appear?

use std::io::{stdin, BufRead};
use std::str::FromStr;
use std::collections::{HashMap, HashSet};
use itertools::Itertools;

macro_rules! to_char {
    ($t:expr) => {
        {
            if $t.len() == 1 {
                $t.chars().next().unwrap()
            } else {
                panic!("input {:?} is not 1 length", $t)
            }
        }
    }
}

macro_rules! char_iter {
    ($t:expr) => {
        $t
            .split("")
            .filter(|s| s.len() > 0)
            .map(|s| to_char!(s))
    }
}
/*
macro_rules! char_set {
    ($t:expr) => {
        char_iter!($t)
            .collect::<HashSet<char>>()
    };
    () => {
        HashSet::new()
    }
}
*/


//region Seven segment model
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum SevenSegment {
    Zero,
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine
}

impl FromStr for SevenSegment {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let sorted_letters = s
            .chars()
            .sorted()
            .collect::<String>();
        
        match sorted_letters.as_str() {
            "abcefg" => Ok(SevenSegment::Zero),
            "cf" => Ok(SevenSegment::One),
            "acdeg" => Ok(SevenSegment::Two),
            "acdfg" => Ok(SevenSegment::Three),
            "bcdf" => Ok(SevenSegment::Four),
            "abdfg" => Ok(SevenSegment::Five),
            "abdefg" => Ok(SevenSegment::Six),
            "acf" => Ok(SevenSegment::Seven),
            "abcdefg" => Ok(SevenSegment::Eight),
            "abcdfg" => Ok(SevenSegment::Nine),
            _ => Err("Not a valid sequence"),
        }
    }
}
//endregion

/// Uses length to determine the corresponding symbol for a 7-seg message
fn determine_seven_segment(s: &str) -> Option<SevenSegment> {
    match s.len() {
        2 => Some(SevenSegment::One),
        3 => Some(SevenSegment::Seven),
        4 => Some(SevenSegment::Four),
        7 => Some(SevenSegment::Eight),
        _ => None,
    }
}

// Algorithm:
// First, we go over the shortest strings:
// #    two    #
// cf means one, so we know any two-wide 7seg input would equal "one"
// eg. "ba" means one, but we don't know which is which.
// we mark down that b could be c or f, and a could be c or f
// #   seven   #
// we go over the next length (3-wide) and determine which of the three is new
// that one is "a"
// eg. "bad" means 7, "ba" being either c or f, so "d" is a
// #   four    #
// we go over the next length (4-wide) and determine which of the two are new
// eg. "bafg" means 4, therefore "f" is b or d, "g" is b or d
// #   

//region Interpolator

#[derive(Default, Debug)]
struct SevenSegmentInterpolator { 
    pub examples: Vec<String>,
    pub appeared_in: HashMap<char, HashSet<SevenSegment>>,
    pub digits: Vec<String>,
}

impl SevenSegmentInterpolator {
    pub fn new() -> Self {
        let mut map: HashMap<char, HashSet<SevenSegment>> = HashMap::new();
        map.insert('a', HashSet::new());
        map.insert('b', HashSet::new());
        map.insert('c', HashSet::new());
        map.insert('d', HashSet::new());
        map.insert('e', HashSet::new());
        map.insert('f', HashSet::new());
        map.insert('g', HashSet::new());

        Self {
            appeared_in: map,
            digits: vec![],
            examples: vec![],
        }
    }
    pub fn from_command(command: &InputCommand) -> Self {
        let mut result = Self::new();

        result.examples = command.examples.clone();

        for example in &command.examples {
            result.accept_example(example);
        }

        for digit in &command.digits {
            result.digits.push(digit.to_string());
        }

        result
    }

    fn accept_example(&mut self, example: &str) {
        // 1. save which knowns it appears in
        let num = determine_seven_segment(example);
        match num {
            Some(n) => {
                for ch in char_iter!(example) {
                    if let Some(set) = self.appeared_in.get_mut(&ch) {
                        (*set).insert(n.clone());
                    }
                }
            },
            _ => {}
        }
    }

    // A: appears in 0, 2, 3, 5, 6, 7, 8, 9
    // B: appears in 0, 4, 5, 6, 8, 9
    // C: appears in 0, 1, 2, 3, 4, 7, 8, 9
    // D: appears in 2, 3, 4, 5, 6, 8, 9
    // E: appears in 0, 2, 6, 8
    // F: appears in 0, 1, 3, 4, 5, 6, 7, 8, 9
    // G: appears in 0, 2, 3, 5, 6, 8, 9


}
//endregion

//region Input parsing
#[derive(Debug, PartialEq)]
struct InputCommand {
    pub examples: Vec<String>,
    pub digits: Vec<String>,
}

impl InputCommand {
    pub fn new(
        examples: Vec<String>,
        digits: Vec<String>
    ) -> Self {
        Self {
            examples,
            digits
        }
    }
}

impl FromStr for InputCommand {
    type Err = &'static str;
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        // abdgfk dg dsgi sgi | fkfksd osdgo
        // => ["abdgfk dg dsgi sgi ", " fkfksd osdgo"]

        let arr = value
                .split("|")
                .map(|x|x.to_owned())
                .collect::<Vec<String>>();

        // "abdgfk dg dsgi sgi "
        // => ["abdgfk", "dg", "dsgi", "sgi"]
        let mut left = arr[0]
                .split(" ")
                .filter(|x| x.len() > 0)
                .map(|x|x.to_owned())
                .collect::<Vec<String>>();

        // ["abdgfk", "dg", "dsgi", "sgi"]
        // => ["dg", "sgi", "dsgi", "abdgfk"]
        left.sort_by(|a, b| {
            use std::cmp::Ordering;

            if a.len() > b.len() {
                return Ordering::Greater;
            }
            if a.len() < b.len() {
                return Ordering::Less;
            }

            b.cmp(a)
        });

        // " fkfksd osdgo"
        // => ["fkfksd", "osdgo"]
        let right = arr[1]
            .split(" ")
            .filter(|x| x.len() > 0)
            .map(|x|x.to_owned())
            .collect::<Vec<String>>();
        
        Ok(
            InputCommand::new(left, right)
        )
    }
}
//endregion


fn commands(stream: impl BufRead) -> Vec<InputCommand> {
    stream
        .lines()
        .filter(|value| value.is_ok())
        .map(|value| value.unwrap())
        .map(|value| value.parse::<InputCommand>().unwrap())
        .collect::<Vec<InputCommand>>()
}

fn main() {

    // part 1: 
    /*
    let mut count = 0;
    for command in commands(stdin().lock()) {
        count += command.digits
            .iter()
            .filter(|digit| determine_seven_segment(digit).is_some())
            .count();
    }

    println!("count is: {}", count);
    */

    for command in commands(stdin().lock()) {
        println!("command: {:?}", command);
        let seg = SevenSegmentInterpolator::from_command(&command);

        println!("{:?}", seg);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn commands_works() {
        let mut input = b"acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf\n" as &[u8];
        
        let command = commands(&mut input);

        assert_eq!(command[0], InputCommand::new(
            vec![
                "ab",
                "dab",
                "eafb",
                "gcdfa",
                "fbcad",
                "cdfbe",
                "cefabd",
                "cdfgeb",
                "cagedb",
                "acedgfb",
            ].into_iter().map(|x|x.to_owned()).collect::<Vec<String>>(),
            vec!["cdfeb", "fcadb", "cdfeb", "cdbaf"].into_iter().map(|x|x.to_owned()).collect::<Vec<String>>()
        ));
    }
}