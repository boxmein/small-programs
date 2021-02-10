// Takes a newline separated list of strings in standard input.
// Outputs strings in standard output where for each string:
// no exclamation mark
// exclamation mark in the start
// exclamation mark in the end

use std::io::{self, BufReader, BufRead};
use std::iter::Iterator;

struct ExclamationMarkPermuter {
    // Stores the string we have to permute
    base_string: String,
    // Stores the iteration step used to derive the new string
    iteration_step: usize,
}

impl ExclamationMarkPermuter {
    fn new(base_string: String) -> ExclamationMarkPermuter {
        ExclamationMarkPermuter {
            iteration_step: 0,
            base_string: base_string.to_string(),
        }
    }

    // Returns the amount of permutations that will be generated.
    fn get_iteration_max(&self) -> usize {
        3
    }

    // Returns the base string permuted by adding an exclamation mark to the start or end.
    fn get_permuted_str(&self) -> String {
        if self.iteration_step == 0 {
          String::from(self.base_string.as_str()) + "!"
        } else if self.iteration_step == 1 {
          String::from("!") + self.base_string.as_str()
        } else {
          String::from(self.base_string.as_str())
        }
    }
}

// Rusty iterator support for the permutation thing
impl Iterator for ExclamationMarkPermuter {
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
    let permuter = ExclamationMarkPermuter::new(line_str);
    for permutation in permuter {
      println!("{}", permutation);
    }
  }
}

#[cfg(test)]
mod tests {
    use super::ExclamationMarkPermuter;
    #[test]
    fn test_it_saves_base_string() {
        let permuter = ExclamationMarkPermuter::new("A".to_string());
        assert_eq!(permuter.base_string, "A".to_string());
    }
    #[test]
    fn test_it_initializes_count_to_0() {
        let permuter = ExclamationMarkPermuter::new("A".to_string());
        assert_eq!(permuter.iteration_step, 0);
    }

    #[test]
    fn test_next_increments_count() {
        let mut permuter = ExclamationMarkPermuter::new("A".to_string());
        assert_eq!(permuter.iteration_step, 0);
        permuter.next();
        assert_eq!(permuter.iteration_step, 1);
    }

    #[test]
    fn next_returns_permuted_string() {
        let mut permuter = ExclamationMarkPermuter::new("AA".to_string());
        let result = permuter.next().unwrap();
        assert_eq!(result, "AA!".to_string());
        let result = permuter.next().unwrap();
        assert_eq!(result, "!AA".to_string());
        let result = permuter.next().unwrap();
        assert_eq!(result, "AA".to_string());
    }

    #[test]
    fn test_it_gets_size_right() {
        let permuter = ExclamationMarkPermuter::new("A".to_string());
        assert_eq!(permuter.get_iteration_max(), 3);
        let permuter = ExclamationMarkPermuter::new("AA".to_string());
        assert_eq!(permuter.get_iteration_max(), 3);
        let permuter = ExclamationMarkPermuter::new("AAA".to_string());
        assert_eq!(permuter.get_iteration_max(), 3);
        let permuter = ExclamationMarkPermuter::new("AAAA".to_string());
        assert_eq!(permuter.get_iteration_max(), 3);
        let permuter = ExclamationMarkPermuter::new("AAAAA".to_string());
        assert_eq!(permuter.get_iteration_max(), 3);
        let permuter = ExclamationMarkPermuter::new("AAAAAA".to_string());
        assert_eq!(permuter.get_iteration_max(), 3);
    }
}
