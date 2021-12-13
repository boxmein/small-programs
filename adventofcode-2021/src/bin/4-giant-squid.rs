//! --- Day 4: Giant Squid ---
// You're already almost 1.5km (almost a mile) below the surface of the ocean, already so deep that you can't see any sunlight. What you can see, however, is a giant squid that has attached itself to the outside of your submarine.
// Maybe it wants to play bingo?
// Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. Numbers are chosen at random, and the chosen number is marked on all boards on which it appears. (Numbers may not appear on all boards.) If all numbers in any row or any column of a board are marked, that board wins. (Diagonals don't count.)
// The submarine has a bingo subsystem to help passengers (currently, you and the giant squid) pass the time. It automatically generates a random order in which to draw numbers and a random set of boards (your puzzle input). For example:
// 7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
// 22 13 17 11  0
//  8  2 23  4 24
// 21  9 14 16  7
//  6 10  3 18  5
//  1 12 20 15 19
//  3 15  0  2 22
//  9 18 13 17  5
// 19  8  7 25 23
// 20 11 10 24  4
// 14 21 16 12  6
// 14 21 17 24  4
// 10 16 15  9 19
// 18  8 23 26 20
// 22 11 13  6  5
//  2  0 12  3  7
// After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no winners, but the boards are marked as follows (shown here adjacent to each other to save space):
// 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
//  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
// 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
//  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
//  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
// After the next six numbers are drawn (17, 23, 2, 0, 14, and 21), there are still no winners:
// 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
//  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
// 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
//  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
//  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
// Finally, 24 is drawn:
// 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
//  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
// 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
//  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
//  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
// At this point, the third board wins because it has at least one complete row or column of marked numbers (in this case, the entire top row is marked: 14 21 17 24 4).
// The score of the winning board can now be calculated. Start by finding the sum of all unmarked numbers on that board; in this case, the sum is 188. Then, multiply that sum by the number that was just called when the board won, 24, to get the final score, 188 * 24 = 4512.
// To guarantee victory against the giant squid, figure out which board will win first. What will your final score be if you choose that board?

// -------

// PART 2 solved in this file by simply not stopping after the first win!

// --- Part Two ---

// On the other hand, it might be wise to try a different strategy: let the giant squid win.

// You aren't sure how many bingo boards a giant squid could play at once, so rather than waste time counting its arms, the safe thing to do is to figure out which board will win last and choose that one. That way, no matter which boards it picks, it will win for sure.

// In the above example, the second board is the last to win, which happens after 13 is eventually called and its middle column is completely marked. If you were to keep playing until this point, the second board would have a sum of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.

// Figure out which board will win last. Once it wins, what would its final score be?

use std::io::{stdin, BufRead};
use std::fmt;
use itertools::Itertools;

#[derive(Debug, Default)]
struct BingoField {
    pub value: i32,
    pub marked: bool,
}

impl BingoField {
    pub fn new(value: i32) -> Self {
        BingoField {
            value, 
            marked: false,
        }
    }
}

impl fmt::Display for BingoField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.marked {
            write!(f, "\x1b[2m{:>3}\x1b[0m", self.value)
        } else {
            write!(f, "{:>3}", self.value)
        }
    }
}

#[derive(Debug)]
struct BingoBoard {
    pub numbers: Vec<Vec<BingoField>>,
    pub last_marked: i32,
}

impl BingoBoard {
    pub fn accept(&mut self, num: i32) -> bool {
        if self.mark_number(num) {
            self.last_marked = num;
        }

        self.wins()
    }

    pub fn wins(&self) -> bool {
        self.any_row_wins() ||
        self.any_column_wins() // || 
        // self.diagonal_ltr_wins() ||
        // self.diagonal_rtl_wins()
    }

    fn any_row_wins(&self) -> bool {
        assert!(self.numbers.len() > 0);
        for row in &self.numbers {
            let mut col_has_unmarked = false;


            for col in row {
                if !col.marked {
                    col_has_unmarked = true;
                }
            }
            if col_has_unmarked {
                continue;
            }
            return true;
        }
        return false;
    }

    fn any_column_wins(&self) -> bool {
        assert!(self.numbers.len() > 0);
        let width = 5;
        for col in 0..width {

            let mut col_has_unmarked = false;
            for row in &self.numbers {
                if !row[col].marked {
                    col_has_unmarked = true;
                }
            }
            
            if col_has_unmarked {
                continue;
            }
            return true;
        }
        return false;
    }

    // fn diagonal_ltr_wins(&self) -> bool {
    //     let n = &self.numbers;
        
    //     n[0][0].marked &&
    //     n[1][1].marked &&
    //     n[2][2].marked &&
    //     n[3][3].marked &&
    //     n[4][4].marked
    // }


    // fn diagonal_rtl_wins(&self) -> bool {
    //     let n = &self.numbers;
        
    //     n[0][4].marked &&
    //     n[1][3].marked &&
    //     n[2][2].marked &&
    //     n[3][1].marked &&
    //     n[4][0].marked
    // }


    pub fn score(&self) -> i32 {
        self.unmarked_sum() * self.last_marked
    }

    fn unmarked_sum(&self) -> i32 {
        let mut sum = 0;
        for row in &self.numbers {
            for col in row {
                if !col.marked {
                    sum += col.value;
                }
            }
        }
        sum
    }

    fn mark_number(&mut self, num: i32) -> bool {
        for row in &mut self.numbers {
            for col in row {
                if col.value == num {
                    col.marked = true;
                    return true;
                }
            }
        }
        return false;
    }

    pub fn new(board: &[&str]) -> Self {
        assert_eq!(board.len(), 5);

        let board: Vec<Vec<BingoField>> = 
            board.iter()
                .map(|s| {
                    let result = s.split_whitespace()
                        .into_iter()
                        .map(|s| s.parse::<i32>().expect("int parsing error"))
                        .map(|i| BingoField::new(i))
                        .collect::<Vec<BingoField>>();
                    assert_eq!(result.len(), 5);

                    result
                })
                .collect::<Vec<Vec<BingoField>>>();
        
        BingoBoard {
            numbers: board,
            last_marked: 0,
        }
    }
}

impl fmt::Display for BingoBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in &self.numbers {
            for col in row {
                write!(f, "{}", col).unwrap();
            }
            write!(f, "\n").unwrap();
        }
        Ok(())
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
    let inp = stdin();
    let locked_stdin = inp.lock();
    let mut iter = commands(locked_stdin);

    // take the first line as vec of i32
    let marks: Vec<i32> = iter.next()
        .expect("at least 1 line in input")
        .split(",")
        .into_iter()
        .map(|item| item.parse::<i32>().expect("mark not i32"))
        .collect::<Vec<i32>>();

    // consume bingo boards 
    let mut boards = iter.batching(|it| {
        // empty line 
        match it.next() {
            // end of sequence: end of bingo boards
            None => None,
            Some(_) => {
                println!("consumed empty line, consuming 5 bingo lines");
                // expect 5 lines
                let first = it.next().expect("no first bingo line");
                let second = it.next().expect("no second bingo line");
                let third = it.next().expect("no third bingo line");
                let fourth = it.next().expect("no fourth bingo line");
                let fifth = it.next().expect("no fifth bingo line");

                println!("consumed 5 bingo lines");

                Some(BingoBoard::new(&[
                    &first,
                    &second,
                    &third,
                    &fourth,
                    &fifth,
                ]))
            }
        } 
    }).collect::<Vec<BingoBoard>>();

    println!("current bingoboards:");

    for board in &boards {
        println!("{}", board);
    }

    // apply all marks to all bingo boards;
    for mark in marks {
        for board in &mut boards {
            if board.wins() {
                continue;
            }
            if board.accept(mark) {
                let score = board.score();
                println!("board wins with score: {}", score);
                // 4.2 solution: comment this line
            }

            println!("Marked ({}) board: \n{}", mark, board);
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test() {

    }
}