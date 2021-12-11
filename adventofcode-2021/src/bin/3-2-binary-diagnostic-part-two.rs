//! --- Day 3: Binary Diagnostic ---
// --- Part Two ---

// Next, you should verify the life support rating, which can be determined by multiplying the oxygen generator rating by the CO2 scrubber rating.

// Both the oxygen generator rating and the CO2 scrubber rating are values that can be found in your diagnostic report - finding them is the tricky part. Both values are located using a similar process that involves filtering out values until only one remains. Before searching for either rating value, start with the full list of binary numbers from your diagnostic report and consider just the first bit of those numbers. Then:

// Keep only numbers selected by the bit criteria for the type of rating value for which you are searching. Discard numbers which do not match the bit criteria.
// If you only have one number left, stop; this is the rating value for which you are searching.
// Otherwise, repeat the process, considering the next bit to the right.
// The bit criteria depends on which type of rating value you want to find:

// To find oxygen generator rating, determine the most common value (0 or 1) in the current bit position, and keep only numbers with that bit in that position. If 0 and 1 are equally common, keep values with a 1 in the position being considered.
// To find CO2 scrubber rating, determine the least common value (0 or 1) in the current bit position, and keep only numbers with that bit in that position. If 0 and 1 are equally common, keep values with a 0 in the position being considered.
// For example, to determine the oxygen generator rating value using the same example diagnostic report from above:

// Start with all 12 numbers and consider only the first bit of each number. There are more 1 bits (7) than 0 bits (5), so keep only the 7 numbers with a 1 in the first position: 11110, 10110, 10111, 10101, 11100, 10000, and 11001.
// Then, consider the second bit of the 7 remaining numbers: there are more 0 bits (4) than 1 bits (3), so keep only the 4 numbers with a 0 in the second position: 10110, 10111, 10101, and 10000.
// In the third position, three of the four numbers have a 1, so keep those three: 10110, 10111, and 10101.
// In the fourth position, two of the three numbers have a 1, so keep those two: 10110 and 10111.
// In the fifth position, there are an equal number of 0 bits and 1 bits (one each). So, to find the oxygen generator rating, keep the number with a 1 in that position: 10111.
// As there is only one number left, stop; the oxygen generator rating is 10111, or 23 in decimal.
// Then, to determine the CO2 scrubber rating value from the same example above:

// Start again with all 12 numbers and consider only the first bit of each number. There are fewer 0 bits (5) than 1 bits (7), so keep only the 5 numbers with a 0 in the first position: 00100, 01111, 00111, 00010, and 01010.
// Then, consider the second bit of the 5 remaining numbers: there are fewer 1 bits (2) than 0 bits (3), so keep only the 2 numbers with a 1 in the second position: 01111 and 01010.
// In the third position, there are an equal number of 0 bits and 1 bits (one each). So, to find the CO2 scrubber rating, keep the number with a 0 in that position: 01010.
// As there is only one number left, stop; the CO2 scrubber rating is 01010, or 10 in decimal.
// Finally, to find the life support rating, multiply the oxygen generator rating (23) by the CO2 scrubber rating (10) to get 230.

// Use the binary numbers in your diagnostic report to calculate the oxygen generator rating and CO2 scrubber rating, then multiply them together. What is the life support rating of the submarine? (Be sure to represent your answer in decimal, not binary.)



use std::io::{stdin, BufRead};

type IntType = i32;
type UnsignedIntType = u32;
const INT_SIZE: usize = 32;

trait BitAt {
    fn bit_at(&self, bit_number: usize) -> i32;
}

impl BitAt for i32 {
    fn bit_at(&self, bit_number: usize) -> i32 {
        (self >> bit_number) & 1
    }
}

struct BinaryDiagnostic {
    pub bit_count: usize,
    pub values: Vec<IntType>,
}

impl BinaryDiagnostic {
    pub fn new() -> Self {
        BinaryDiagnostic {
            bit_count: 0,
            values: vec![],
        }
    }
    pub fn set_bit_count(&mut self, bit_count: usize) {
        self.bit_count = bit_count;
    }
    pub fn accept(&mut self, inp: IntType) {
        self.values.push(inp);
    }

    pub fn oxygen_generator_rating(&self) -> IntType {
        recursive_filter(
            &self.values,
            self.bit_count - 1,
            &Commonality::MostCommon,
            1
        )
    }

    pub fn co2_scrubber_rating(&self) -> IntType {
        recursive_filter(
            &self.values,
            self.bit_count - 1,
            &Commonality::LeastCommon,
            0
        )
    }
}

enum Commonality {
    MostCommon,
    LeastCommon
}

fn recursive_filter(
    values: &[i32],
    start_bit: usize,
    commonality: &Commonality,
    tie_breaker: i32
) -> i32 {
    if values.len() == 0 {
        panic!("did not find it!");
    }
    if values.len() == 1 {
        return values[0];
    }

    assert!(tie_breaker >= 0 && tie_breaker <= 1);
    assert!(start_bit <= INT_SIZE - 1);

    let common = commonality_bit_for_values(values, commonality, start_bit, tie_breaker);

    let new_values = values
        .iter()
        .filter(|value| value.bit_at(start_bit) == common)
        .cloned()
        .collect::<Vec<i32>>();

    println!("iteration: common = {}, count = {}", common, new_values.len());

    if new_values.len() == 1 {
        return new_values[0];
    }

    assert!(start_bit > 0);

    return recursive_filter(&new_values, start_bit - 1, commonality, tie_breaker);
}

fn commonality_bit_for_values(values: &[i32], commonality: &Commonality, bit_position: usize, tie_breaker: i32) -> i32 {
    match commonality {
        Commonality::LeastCommon => least_common_bit_for_values(values, bit_position, tie_breaker),
        Commonality::MostCommon => most_common_bit_for_values(values, bit_position, tie_breaker)
    }
}

fn most_common_bit_for_values(values: &[i32], bit_position: usize, tie_breaker: i32) -> IntType {
    let mut zero_count = 0;
    let mut one_count = 0;

    for item in values {
        if item.bit_at(bit_position) == 1 {
            one_count += 1;
        } else {
            zero_count += 1;
        }
    }

    if zero_count > one_count {
        0
    } else if zero_count < one_count {
        1
    } else {
        tie_breaker
    }
}

fn least_common_bit_for_values(values: &[i32], bit_position: usize, tie_breaker: i32) -> IntType {
    let mut zero_count = 0;
    let mut one_count = 0;

    for item in values {
        if item.bit_at(bit_position) == 1 {
            one_count += 1;
        } else {
            zero_count += 1;
        }
    }

    if zero_count > one_count {
        1
    } else if zero_count < one_count {
        0
    } else {
        tie_breaker
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
    let mut diagnosticator = BinaryDiagnostic::new();
    let mut bit_count: usize = 0;
    for command in commands(stdin()
    .lock()) {
        let command_length = command.len();
        if command_length > bit_count {
            bit_count = command_length;
            diagnosticator.set_bit_count(bit_count);
        }
        let val = IntType::from_str_radix(&command, 2).expect("cannot parse as int32");
        diagnosticator.accept(val);
    }

    let oxygen: IntType = diagnosticator.oxygen_generator_rating();
    let co2: IntType = diagnosticator.co2_scrubber_rating();

    println!("Oxygen: {}", oxygen);
    println!("CO2:   {}", co2); 
    println!("Life support rating: {}", oxygen * co2);
}


#[cfg(test)]
mod tests {
    use super::{BitAt, most_common_bit_for_values, least_common_bit_for_values, Commonality, recursive_filter};
    #[test]
    fn bit_at_works() {
        let i: i32 = 0b11001100;

        assert_eq!(i.bit_at(0), 0);
        assert_eq!(i.bit_at(1), 0);
        assert_eq!(i.bit_at(2), 1);
        assert_eq!(i.bit_at(3), 1);
        assert_eq!(i.bit_at(4), 0);
        assert_eq!(i.bit_at(5), 0);
        assert_eq!(i.bit_at(6), 1);
        assert_eq!(i.bit_at(7), 1);
    }

    #[test]
    fn most_common_bit_works() {
        let values: Vec<i32> = vec![
            0b1100,
            0b1101,
            0b1101,
            0b1101,
            0b1101,
            0b1100,
            0b1100,
            0b1011,
            //   ^ -> 1
        ];

        let result = most_common_bit_for_values(&values, 0, 0);

        assert_eq!(result, 1);
    }

    #[test]
    fn most_common_bit_tie_breaker() {
        let values: Vec<i32> = vec![
            0b1100,
            0b1101,
            0b1101,
            0b1101,
            0b1101,
            0b1100,
            0b1100,
            0b1010,
            //   ^ -> tied! tie breaker is returned
        ];

        let result = most_common_bit_for_values(&values, 0, 2);

        assert_eq!(result, 2);
    }


    #[test]
    fn least_common_bit_works() {
        let values: Vec<i32> = vec![
            0b1100,
            0b1101,
            0b1101,
            0b1101,
            0b1101,
            0b1100,
            0b1100,
            0b1011,
            //   ^ -> 0
        ];

        let result = least_common_bit_for_values(&values, 0, 0);

        assert_eq!(result, 0);
    }

    #[test]
    fn least_common_bit_tie_breaker() {
        let values: Vec<i32> = vec![
            0b1100,
            0b1101,
            0b1101,
            0b1101,
            0b1101,
            0b1100,
            0b1100,
            0b1010,
            //   ^ -> tied! tie breaker is returned
        ];

        let result = least_common_bit_for_values(&values, 0, 2);

        assert_eq!(result, 2);
    }

    #[test]
    fn recursive_filter_most_common() {
        let values: Vec<i32> = vec![
                     // Rounds:
                     // 1  2  3
            0b11111, // +
            0b10000, // +  +  
            0b10101, // +  +  +  winner
            0b01100, // .
            0b01101, // .
        ];

        let start_bit = 4;
        let tie_breaker = 1;
        let commonality = Commonality::MostCommon;

        let result = recursive_filter(
            &values,
            start_bit,
            &commonality,
            tie_breaker,
        );

        assert_eq!(result, 0b10101)
    }

    #[test]
    fn recursive_filter_least_common() {
        let values: Vec<i32> = vec![
                     // Rounds:
                     // 1  2  3
            0b10111, // .
            0b10111, // .
            0b10111, // .
            0b10000, // .  
            0b00001, // +  +  +  winner
            0b01100, // +  +  
            0b01101, // +
        ];

        let start_bit = 4;
        let tie_breaker = 1;
        let commonality = Commonality::LeastCommon;

        let result = recursive_filter(
            &values,
            start_bit,
            &commonality,
            tie_breaker,
        );

        assert_eq!(result, 0b00001);
    }
}