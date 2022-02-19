//! --- Day 14: Extended Polymerization ---
// The incredible pressures at this depth are starting to put a strain on your submarine. The submarine has polymerization equipment that would produce suitable materials to reinforce the submarine, and the nearby volcanically-active caves should even have the necessary input elements in sufficient quantities.

// The submarine manual contains instructions for finding the optimal polymer formula; specifically, it offers a polymer template and a list of pair insertion rules (your puzzle input). You just need to work out what polymer would result after repeating the pair insertion process a few times.

// For example:

// NNCB

// CH -> B
// HH -> N
// CB -> H
// NH -> C
// HB -> C
// HC -> B
// HN -> C
// NN -> C
// BH -> H
// NC -> B
// NB -> B
// BN -> B
// BB -> N
// BC -> B
// CC -> N
// CN -> C

// The first line is the polymer template - this is the starting point of the process.

// The following section defines the pair insertion rules. A rule like AB -> C means that when elements A and B are immediately adjacent, element C should be inserted between them. These insertions all happen simultaneously.

// So, starting with the polymer template NNCB, the first step simultaneously considers all three pairs:

//     The first pair (NN) matches the rule NN -> C, so element C is inserted between the first N and the second N.
//     The second pair (NC) matches the rule NC -> B, so element B is inserted between the N and the C.
//     The third pair (CB) matches the rule CB -> H, so element H is inserted between the C and the B.

// Note that these pairs overlap: the second element of one pair is the first element of the next pair. Also, because all pairs are considered simultaneously, inserted elements are not considered to be part of a pair until the next step.

// After the first step of this process, the polymer becomes NCNBCHB.

// Here are the results of a few steps using the above rules:

// Template:     NNCB
// After step 1: NCNBCHB
// After step 2: NBCCNBBBCBHCB
// After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
// After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB

// This polymer grows quickly. After step 5, it has length 97; After step 10, it has length 3073. After step 10, B occurs 1749 times, C occurs 298 times, H occurs 161 times, and N occurs 865 times; taking the quantity of the most common element (B, 1749) and subtracting the quantity of the least common element (H, 161) produces 1749 - 161 = 1588.

// Apply 10 steps of pair insertion to the polymer template and find the most and least common elements in the result. What do you get if you take the quantity of the most common element and subtract the quantity of the least common element?

// Your puzzle answer was 3213.

// --- Part Two ---

// The resulting polymer isn't nearly strong enough to reinforce the submarine. You'll need to run more steps of the pair insertion process; a total of 40 steps should do it.

// In the above example, the most common element is B (occurring 2192039569602 times) and the least common element is H (occurring 3849876073 times); subtracting these produces 2188189693529.

// Apply 40 steps of pair insertion to the polymer template and find the most and least common elements in the result. What do you get if you take the quantity of the most common element and subtract the quantity of the least common element?

use std::collections::HashMap;
use std::fmt;
use std::io::{stdin, BufRead};
use std::str::FromStr;

#[derive(Debug)]
struct Polymer {
    pub data: String,
    commonality: HashMap<char, usize>,
}

impl Polymer {
    pub fn new(data: &str) -> Self {
        Self {
            data: data.to_string(),
            commonality: HashMap::new(),
        }
    }

    pub fn build_commonality(&mut self) {
        for letter in self.data.chars() {
            if !self.commonality.contains_key(&letter) {
                self.commonality.insert(letter, 0);
            }

            if let Some(x) = self.commonality.get_mut(&letter) {
                *x += 1;
            } else {
                panic!("line 25 did not initialize for letter {}", letter);
            }
        }
    }

    pub fn most_common_letter(&mut self) -> (char, usize) {
        if self.commonality.len() == 0 {
            self.build_commonality();
        }

        let mut max_letter: char = '-';
        let mut max_commonality: usize = 0;

        for (letter, count) in self.commonality.iter() {
            if *count > max_commonality {
                max_commonality = *count;
                max_letter = *letter;
            }
        }

        (max_letter, max_commonality)
    }

    pub fn least_common_letter(&mut self) -> (char, usize) {
        if self.commonality.len() == 0 {
            self.build_commonality();
        }

        let mut min_letter: char = '-';
        let mut min_commonality: usize = usize::MAX;

        for (letter, count) in self.commonality.iter() {
            if *count < min_commonality {
                min_commonality = *count;
                min_letter = *letter;
            }
        }

        (min_letter, min_commonality)
    }
}

impl fmt::Display for Polymer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.data)
    }
}

impl FromStr for Polymer {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Polymer::new(s))
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Rule {
    before: String,
    insert: char,
}

impl Rule {
    pub fn applies_to_pair(&self, pair: &str) -> bool {
        self.before == pair
    }
}

#[derive(Debug)]
struct RuleParseError;

impl FromStr for Rule {
    type Err = RuleParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split(' ').collect();

        assert_eq!(parts[1], "->");

        Ok(Self {
            before: parts[0].to_string(),
            insert: parts[2].chars().next().unwrap(),
        })
    }
}

fn apply_rules(polymer: Polymer, rules: &[Rule]) -> Polymer {
    let mut cursor = 0;
    let mut chars = polymer.data.chars().collect::<Vec<char>>();

    loop {
        let mut insertion_happened = false;
        if cursor >= chars.len() - 1 {
            break;
        }

        let pair: String = vec![chars[cursor], chars[cursor + 1]].into_iter().collect();

        for rule in rules {
            if !rule.applies_to_pair(&pair) {
                continue;
            }

            chars.insert(cursor + 1, rule.insert);
            insertion_happened = true;
        }

        if insertion_happened {
            cursor += 2;
        } else {
            cursor += 1;
        }
    }

    Polymer::new(&chars.into_iter().collect::<String>())
}

fn main() {
    let input = stdin();
    let mut lines = input
        .lock()
        .lines()
        .map(|value| value.expect("stdin failure"))
        .filter(|value| value != "");

    let polymer = Polymer::new(&lines.next().unwrap());

    let rules: Vec<Rule> = lines.map(|line| line.parse::<Rule>().unwrap()).collect();

    println!("{}", polymer);

    let mut new_polymer = polymer;
    for i in 0..10 {
        new_polymer = apply_rules(new_polymer, &rules);
        println!("#{:<3}: {}", i, new_polymer);
    }

    let (max_letter, max_count) = new_polymer.most_common_letter();
    let (min_letter, min_count) = new_polymer.least_common_letter();

    println!("most common: {} (count: {})", max_letter, max_count);
    println!("least common: {} (count: {})", min_letter, min_count);

    println!(
        "subtract: {} - {} = {}",
        max_count,
        min_count,
        max_count - min_count
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn rule_parses() {
        assert_eq!(
            "CH -> B".parse::<Rule>().unwrap(),
            Rule {
                before: "CH".to_string(),
                insert: 'B',
            }
        );
    }

    #[test]
    fn example_correct() {
        let polymer = "NNCB".parse::<Polymer>().unwrap();
        let rules: Vec<Rule> = vec![
            "CH -> B", "HH -> N", "CB -> H", "NH -> C", "HB -> C", "HC -> B", "HN -> C", "NN -> C",
            "BH -> H", "NC -> B", "NB -> B", "BN -> B", "BB -> N", "BC -> B", "CC -> N", "CN -> C",
        ]
        .into_iter()
        .map(|r| r.parse::<Rule>().unwrap())
        .collect();

        let mut new = polymer;
        for i in 0..10 {
            new = apply_rules(new, &rules);
            if i == 0 {
                assert_eq!(new.data, "NCNBCHB");
            } else if i == 1 {
                assert_eq!(new.data, "NBCCNBBBCBHCB");
            } else if i == 2 {
                assert_eq!(new.data, "NBBBCNCCNBBNBNBBCHBHHBCHB");
            } else if i == 3 {
                assert_eq!(
                    new.data,
                    "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
                );
            } else if i == 4 {
                assert_eq!(new.data.len(), 97);
            } else if i == 9 {
                assert_eq!(new.data.len(), 3073);
            }
        }

        let (max_letter, max_count) = new.most_common_letter();
        let (min_letter, min_count) = new.least_common_letter();

        assert_eq!(max_letter, 'B');
        assert_eq!(max_count, 1749);
        assert_eq!(min_letter, 'H');
        assert_eq!(min_count, 161);
        assert_eq!(max_count - min_count, 1588);
    }

    #[test]
    fn most_common_letter_correct() {
        let mut polymer = "NNCB".parse::<Polymer>().unwrap();

        let (letter, count) = polymer.most_common_letter();

        assert_eq!(letter, 'N');
        assert_eq!(count, 2);
    }

    #[test]
    fn least_common_letter_correct() {
        let mut polymer = "NNCCCB".parse::<Polymer>().unwrap();

        let (letter, count) = polymer.least_common_letter();

        assert_eq!(letter, 'B');
        assert_eq!(count, 1);
    }

    #[test]
    fn least_common_letter_takes_random_if_multiple() {
        let mut polymer = "NNCB".parse::<Polymer>().unwrap();

        let (letter, count) = polymer.least_common_letter();

        assert!(letter == 'C' || letter == 'B');
        assert_eq!(count, 1);
    }
}
