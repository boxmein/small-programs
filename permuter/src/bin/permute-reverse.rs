// Takes a newline separated list of strings in standard input.
// Outputs strings in standard output where for each string:
// the string itself
// and the reversed string
// is outputted.

use std::io::{self, BufReader, BufRead};
use std::iter::Iterator;

struct ReversePermuter {
    // Stores the string we have to permute
    base_string: String,
    // Stores the iteration step used to derive the new string
    iteration_step: usize,
}

impl ReversePermuter {
    fn new(base_string: String) -> ReversePermuter {
        ReversePermuter {
            iteration_step: 0,
            base_string: base_string.to_string(),
        }
    }

    // Returns the amount of permutations that will be generated.
    fn get_iteration_max(&self) -> usize {
        2
    }

    // Returns the base string permuted by adding an exclamation mark to the start or end.
    fn get_permuted_str(&self) -> String {
        if self.iteration_step == 0 {
          String::from(self.base_string.as_str())
        } else {
          self.base_string.chars().rev().collect::<String>()
        }
    }
}

// Rusty iterator support for the permutation thing
impl Iterator for ReversePermuter {
    type Item = String;
    fn next(&mut self) -> Option<Self::Item> {
        let value = self.get_permuted_str();
        self.iteration_step += 1;

        if self.iteration_step == self.get_iteration_max() + 1 {
            None
        } else {
            Some(value.to_string())
        }
    }
}

fn main() {
  let stdin_reader = BufReader::new(io::stdin()).lines();
  for line in stdin_reader {
    let line_str = line.unwrap();
    let permuter = ReversePermuter::new(line_str);
    for permutation in permuter {
      println!("{}", permutation);
    }
  }
}

#[cfg(test)]
mod tests {
    use super::ReversePermuter;
    #[test]
    fn test_it_saves_base_string() {
        let permuter = ReversePermuter::new("A".to_string());
        assert_eq!(permuter.base_string, "A".to_string());
    }
    #[test]
    fn test_it_initializes_count_to_0() {
        let permuter = ReversePermuter::new("A".to_string());
        assert_eq!(permuter.iteration_step, 0);
    }

    #[test]
    fn test_next_increments_count() {
        let mut permuter = ReversePermuter::new("A".to_string());
        assert_eq!(permuter.iteration_step, 0);
        permuter.next();
        assert_eq!(permuter.iteration_step, 1);
    }

    #[test]
    fn next_returns_permuted_string() {
        let mut permuter = ReversePermuter::new("AB".to_string());
        let result = permuter.next().unwrap();
        assert_eq!(result, "AB".to_string());
        let result = permuter.next().unwrap();
        assert_eq!(result, "BA".to_string());
    }

    #[test]
    fn test_it_gets_size_right() {
        let permuter = ReversePermuter::new("A".to_string());
        assert_eq!(permuter.get_iteration_max(), 2);
        let permuter = ReversePermuter::new("AA".to_string());
        assert_eq!(permuter.get_iteration_max(), 2);
        let permuter = ReversePermuter::new("AAA".to_string());
        assert_eq!(permuter.get_iteration_max(), 2);
        let permuter = ReversePermuter::new("AAAA".to_string());
        assert_eq!(permuter.get_iteration_max(), 2);
        let permuter = ReversePermuter::new("AAAAA".to_string());
        assert_eq!(permuter.get_iteration_max(), 2);
        let permuter = ReversePermuter::new("AAAAAA".to_string());
        assert_eq!(permuter.get_iteration_max(), 2);
    }
}
