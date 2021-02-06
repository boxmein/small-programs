// Takes a newline separated list of strings in standard input.
// Outputs a newline separated list of strings in standard output,
// where: 
//  each input string is permuted with different capitalization

use std::io::{self, BufReader, BufRead};
use std::iter::Iterator;
use std::convert::TryInto;

struct AlphabeticalPermuter {
    // Stores the string we have to permute
    base_string: String,
    // Stores the iteration step used to derive the new string
    iteration_step: usize,
}

impl AlphabeticalPermuter {
    fn new(base_string: String) -> AlphabeticalPermuter {
        AlphabeticalPermuter {
            iteration_step: 0,
            base_string: base_string.to_string(),
        }
    }

    // Returns the amount of permutations that will be generated.
    fn get_iteration_max(&self) -> usize {
        (2_usize).pow(self.base_string.len().try_into().unwrap())
    }

    // Returns true if in the current iteration step, the character
    // with a given index should be upper-cased.
    fn should_be_uppercase(&self, letter_index: &usize) -> bool {
        (self.iteration_step >> letter_index) & 1 == 1
    }

    // Returns the base string permuted by modifying characters' 
    // case to upper or lower.
    // Unicode compatible.
    fn get_permuted_str(&self) -> String {
        let mut new_str = String::with_capacity(self.base_string.len());
        for (index, character) in self.base_string.char_indices() {
            if !character.is_uppercase() && !character.is_lowercase() {
                new_str.push(character);
                continue;
            }
            if self.should_be_uppercase(&index) {
                let uppercased_result = character.to_uppercase(); // may be multiple characters
                for chara in uppercased_result {
                    new_str.push(chara);
                }
            } else {
                let lowercased_result = character.to_lowercase(); // may be multiple characters
                for chara in lowercased_result {
                    new_str.push(chara);
                }
            }
        }

        new_str
    }
}

// Rusty iterator support for the permutation thing
impl Iterator for AlphabeticalPermuter {
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
    let permuter = AlphabeticalPermuter::new(line_str);
    for permutation in permuter {
      println!("{}", permutation);
    }
  }
}

#[cfg(test)]
mod tests {
    use super::AlphabeticalPermuter;
    #[test]
    fn test_it_saves_base_string() {
        let permuter = AlphabeticalPermuter::new("A".to_string());
        assert_eq!(permuter.base_string, "A".to_string());
    }
    #[test]
    fn test_it_initializes_count_to_0() {
        let permuter = AlphabeticalPermuter::new("A".to_string());
        assert_eq!(permuter.iteration_step, 0);
    }

    #[test]
    fn test_next_increments_count() {
        let mut permuter = AlphabeticalPermuter::new("A".to_string());
        assert_eq!(permuter.iteration_step, 0);
        permuter.next();
        assert_eq!(permuter.iteration_step, 1);
    }

    #[test]
    fn next_returns_permuted_string() {
        let mut permuter = AlphabeticalPermuter::new("AA".to_string());
        let result = permuter.next().unwrap();
        assert_eq!(result, "aa".to_string());
        let result = permuter.next().unwrap();
        assert_eq!(result, "Aa".to_string());
        let result = permuter.next().unwrap();
        assert_eq!(result, "aA".to_string());
        let result = permuter.next().unwrap();
        assert_eq!(result, "AA".to_string());
    }

    #[ignore] // this is not implemented yet
    #[test]
    fn next_does_not_return_same_string_for_numbers() {
        let mut permuter = AlphabeticalPermuter::new("A1".to_string());
        let result = permuter.next().unwrap();
        assert_eq!(result, "a1".to_string());
        let result = permuter.next().unwrap();
        assert_eq!(result, "A1".to_string());
        let result = permuter.next();
        assert_eq!(result, None);
    }

    #[test]
    fn test_it_gets_size_right() {
        let permuter = AlphabeticalPermuter::new("A".to_string());
        assert_eq!(permuter.get_iteration_max(), 2);
        let permuter = AlphabeticalPermuter::new("AA".to_string());
        assert_eq!(permuter.get_iteration_max(), 4);
        let permuter = AlphabeticalPermuter::new("AAA".to_string());
        assert_eq!(permuter.get_iteration_max(), 8);
        let permuter = AlphabeticalPermuter::new("AAAA".to_string());
        assert_eq!(permuter.get_iteration_max(), 16);
        let permuter = AlphabeticalPermuter::new("AAAAA".to_string());
        assert_eq!(permuter.get_iteration_max(), 32);
        let permuter = AlphabeticalPermuter::new("AAAAAA".to_string());
        assert_eq!(permuter.get_iteration_max(), 64);
    }
}
