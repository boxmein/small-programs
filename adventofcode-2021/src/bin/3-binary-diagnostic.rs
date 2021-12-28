//! --- Day 3: Binary Diagnostic ---
// The submarine has been making some odd creaking noises, so you ask it to produce a diagnostic report just in case.
// The diagnostic report (your puzzle input) consists of a list of binary numbers which, when decoded properly, can tell you many useful things about the conditions of the submarine. The first parameter to check is the power consumption.
// You need to use the binary numbers in the diagnostic report to generate two new binary numbers (called the gamma rate and the epsilon rate). The power consumption can then be found by multiplying the gamma rate by the epsilon rate.
// Each bit in the gamma rate can be determined by finding the most common bit in the corresponding position of all numbers in the diagnostic report. For example, given the following diagnostic report:
// 00100
// 11110
// 10110
// 10111
// 10101
// 01111
// 00111
// 11100
// 10000
// 11001
// 00010
// 01010
// Considering only the first bit of each number, there are five 0 bits and seven 1 bits. Since the most common bit is 1, the first bit of the gamma rate is 1.
// The most common second bit of the numbers in the diagnostic report is 0, so the second bit of the gamma rate is 0.
// The most common value of the third, fourth, and fifth bits are 1, 1, and 0, respectively, and so the final three bits of the gamma rate are 110.
// So, the gamma rate is the binary number 10110, or 22 in decimal.
// The epsilon rate is calculated in a similar way; rather than use the most common bit, the least common bit from each position is used. So, the epsilon rate is 01001, or 9 in decimal. Multiplying the gamma rate (22) by the epsilon rate (9) produces the power consumption, 198.
// Use the binary numbers in your diagnostic report to calculate the gamma rate and epsilon rate, then multiply them together. What is the power consumption of the submarine? (Be sure to represent your answer in decimal, not binary.)

use std::io::{stdin, BufRead};

type IntType = i32;
type UnsignedIntType = u32;
const INT_SIZE: usize = 32;

struct BinaryDiagnostic {
    pub zero_counts: Vec<IntType>,
    pub one_counts: Vec<IntType>,
}

impl BinaryDiagnostic {
    pub fn new() -> Self {
        let mut zero_counts = Vec::with_capacity(INT_SIZE);
        let mut one_counts = Vec::with_capacity(INT_SIZE);

        for _ in 0..INT_SIZE {
            zero_counts.push(0);
            one_counts.push(0);
        }

        assert!(zero_counts.len() == INT_SIZE);
        assert!(one_counts.len() == INT_SIZE);

        BinaryDiagnostic {
            zero_counts,
            one_counts,
        }
    }
    pub fn accept(&mut self, inp: IntType) {
        for (i, bit) in iterate_over_bits(inp).enumerate() {
            match bit {
                0 => self.zero_counts[i] += 1,
                1 => self.one_counts[i] += 1,
                _ => (),
            }
        }
    }

    pub fn epsilon(&self) -> UnsignedIntType {
        let mut builder = IntBitBuilder::new();

        for i in 0..INT_SIZE {
            let mc = self.most_common_for_bit(i);
            builder.set_bit(i, mc);
        }

        builder.result()
    }

    pub fn gamma(&self) -> UnsignedIntType {
        let mut builder = IntBitBuilder::new();

        for i in 0..INT_SIZE {
            let mc = self.least_common_for_bit(i);
            builder.set_bit(i, mc);
        }

        builder.result()
    }

    pub fn report_gamma_for_bit_count(&self, bit_count: usize) -> String {
        assert!(bit_count <= INT_SIZE);
        let mut gamma_str = "".to_owned();
        for i in 0..bit_count {
            let mc = self.most_common_for_bit(i);
            gamma_str += mc.to_string().as_ref();
        }

        gamma_str.to_string().chars().rev().collect::<String>()
    }

    pub fn report_epsilon_for_bit_count(&self, bit_count: usize) -> String {
        assert!(bit_count <= INT_SIZE);
        let mut epsilon_str = "".to_owned();
        for i in 0..bit_count {
            let mc = self.least_common_for_bit(i as usize);
            epsilon_str += mc.to_string().as_ref();
        }

        epsilon_str.to_string().chars().rev().collect::<String>()
    }

    fn most_common_for_bit(&self, bit_number: usize) -> IntType {
        assert!(bit_number <= INT_SIZE - 1);

        if self.zero_counts[bit_number] == 0 && self.one_counts[bit_number] == 0 {
            return 0;
        }

        if self.zero_counts[bit_number] > self.one_counts[bit_number] {
            0
        } else {
            1
        }
    }

    fn least_common_for_bit(&self, bit_number: usize) -> IntType {
        assert!(bit_number <= INT_SIZE - 1);

        if self.one_counts[bit_number] == 0 {
            return 0;
        }

        let more_zeros_than_ones = self.zero_counts[bit_number] > self.one_counts[bit_number];
        if more_zeros_than_ones {
            1
        } else {
            0
        }
    }
}

struct IntBitIterator {
    value: IntType,
    at: usize,
}

impl IntBitIterator {
    pub fn new(value: IntType) -> Self {
        IntBitIterator { value, at: 0 }
    }

    fn bit_at(&self, bit_number: usize) -> IntType {
        (self.value >> bit_number) & 1
    }
}

impl Iterator for IntBitIterator {
    type Item = IntType;

    fn next(&mut self) -> Option<Self::Item> {
        if self.at > INT_SIZE - 1 {
            return None;
        }

        self.at += 1;
        Some(self.bit_at(self.at - 1))
    }
}

fn iterate_over_bits(i: IntType) -> impl Iterator<Item = IntType> {
    IntBitIterator::new(i)
}

struct IntBitBuilder(UnsignedIntType);

impl IntBitBuilder {
    pub fn new() -> Self {
        IntBitBuilder(0)
    }

    pub fn set_bit(&mut self, position: usize, value: IntType) {
        let mut val = self.0;

        if value == 1 {
            val = val | (1 << position);
        } else {
            val = val & !(1 << position);
        }

        self.0 = val;
    }

    pub fn result(&self) -> UnsignedIntType {
        self.0
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
    for command in commands(stdin().lock()) {
        let command_length = command.len();
        if command_length > bit_count {
            bit_count = command_length;
        }
        let val = IntType::from_str_radix(&command, 2).expect("cannot parse as int32");
        diagnosticator.accept(val);
    }

    let epsilon: UnsignedIntType = diagnosticator.epsilon();
    let gamma: UnsignedIntType = diagnosticator.gamma();

    println!(
        "Epsilon: {} ({})",
        epsilon,
        diagnosticator.report_epsilon_for_bit_count(bit_count)
    );
    println!(
        "Gamma:   {} ({})",
        gamma,
        diagnosticator.report_gamma_for_bit_count(bit_count)
    );
    println!("Result: {}", epsilon * gamma);
}

#[cfg(test)]
mod tests {
    use super::{BinaryDiagnostic, IntBitBuilder, IntBitIterator};
    #[test]
    fn int_bit_iterator_works() {
        let iter = IntBitIterator::new(0b1010110011110000);
        let results = iter.collect::<Vec<i32>>();

        assert_eq!(results[15], 1);
        assert_eq!(results[14], 0);
        assert_eq!(results[13], 1);
        assert_eq!(results[12], 0);
        assert_eq!(results[11], 1);
        assert_eq!(results[10], 1);
        assert_eq!(results[9], 0);
        assert_eq!(results[8], 0);
        assert_eq!(results[7], 1);
        assert_eq!(results[6], 1);
        assert_eq!(results[5], 1);
        assert_eq!(results[4], 1);
        assert_eq!(results[3], 0);
        assert_eq!(results[2], 0);
        assert_eq!(results[1], 0);
        assert_eq!(results[0], 0);
    }

    #[test]
    fn int_bit_builder_works() {
        let mut builder = IntBitBuilder::new();

        builder.set_bit(0, 1);
        builder.set_bit(1, 1);
        builder.set_bit(2, 0);
        builder.set_bit(3, 0);
        builder.set_bit(4, 1);
        builder.set_bit(5, 1);
        builder.set_bit(6, 0);
        builder.set_bit(7, 0);
        builder.set_bit(8, 1);

        let res = builder.result();

        assert_eq!(res, 0b100110011);
    }

    #[test]
    fn binary_diagnostic_accept_works() {
        let mut diag = BinaryDiagnostic::new();

        diag.accept(0b1010);

        assert_eq!(diag.zero_counts[0], 1);
        assert_eq!(diag.one_counts[0], 0);

        assert_eq!(diag.zero_counts[1], 0);
        assert_eq!(diag.one_counts[1], 1);

        assert_eq!(diag.zero_counts[2], 1);
        assert_eq!(diag.one_counts[2], 0);

        assert_eq!(diag.zero_counts[3], 0);
        assert_eq!(diag.one_counts[3], 1);

        diag.accept(0b1111);

        assert_eq!(diag.zero_counts[0], 1);
        assert_eq!(diag.one_counts[0], 1);

        assert_eq!(diag.zero_counts[1], 0);
        assert_eq!(diag.one_counts[1], 2);

        assert_eq!(diag.zero_counts[2], 1);
        assert_eq!(diag.one_counts[2], 1);

        assert_eq!(diag.zero_counts[3], 0);
        assert_eq!(diag.one_counts[3], 2);
    }

    #[test]
    fn binary_diagnostic_epsilon_works() {
        let mut diag = BinaryDiagnostic::new();

        diag.accept(0b1000);
        diag.accept(0b1100);
        diag.accept(0b1101);
        diag.accept(0b1110);
        // Most common:
        //            1100
        let eps = diag.epsilon();

        assert_eq!(eps, 0b1100);
    }

    #[test]
    fn binary_diagnostic_gamma_works() {
        let mut diag = BinaryDiagnostic::new();

        diag.accept(0b1000);
        diag.accept(0b1100);
        diag.accept(0b0101);
        diag.accept(0b1110);
        // Least common:
        //            0011
        let gamma = diag.gamma();

        assert_eq!(gamma, 0b0011);
    }
}
