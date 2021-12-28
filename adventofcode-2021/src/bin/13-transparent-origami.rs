// --- Day 13: Transparent Origami ---

// You reach another volcanically active part of the cave. It would be nice if you could do some kind of thermal imaging so you could tell ahead of time which caves are too hot to safely enter.

// Fortunately, the submarine seems to be equipped with a thermal camera! When you activate it, you are greeted with:

// Congratulations on your purchase! To activate this infrared thermal imaging
// camera system, please enter the code found on page 1 of the manual.

// Apparently, the Elves have never used this feature. To your surprise, you manage to find the manual; as you go to open it, page 1 falls out. It's a large sheet of transparent paper! The transparent paper is marked with random dots and includes instructions on how to fold it up (your puzzle input). For example:

// 6,10
// 0,14
// 9,10
// 0,3
// 10,4
// 4,11
// 6,0
// 6,12
// 4,1
// 0,13
// 10,12
// 3,4
// 3,0
// 8,4
// 1,10
// 2,14
// 8,10
// 9,0

// fold along y=7
// fold along x=5

// The first section is a list of dots on the transparent paper. 0,0 represents the top-left coordinate. The first value, x, increases to the right. The second value, y, increases downward. So, the coordinate 3,0 is to the right of 0,0, and the coordinate 0,7 is below 0,0. The coordinates in this example form the following pattern, where # is a dot on the paper and . is an empty, unmarked position:

// ...#..#..#.
// ....#......
// ...........
// #..........
// ...#....#.#
// ...........
// ...........
// ...........
// ...........
// ...........
// .#....#.##.
// ....#......
// ......#...#
// #..........
// #.#........

// Then, there is a list of fold instructions. Each instruction indicates a line on the transparent paper and wants you to fold the paper up (for horizontal y=... lines) or left (for vertical x=... lines). In this example, the first fold instruction is fold along y=7, which designates the line formed by all of the positions where y is 7 (marked here with -):

// ...#..#..#.
// ....#......
// ...........
// #..........
// ...#....#.#
// ...........
// ...........
// -----------
// ...........
// ...........
// .#....#.##.
// ....#......
// ......#...#
// #..........
// #.#........

// Because this is a horizontal line, fold the bottom half up. Some of the dots might end up overlapping after the fold is complete, but dots will never appear exactly on a fold line. The result of doing this fold looks like this:

// #.##..#..#.
// #...#......
// ......#...#
// #...#......
// .#.#..#.###
// ...........
// ...........

// Now, only 17 dots are visible.

// Notice, for example, the two dots in the bottom left corner before the transparent paper is folded; after the fold is complete, those dots appear in the top left corner (at 0,0 and 0,1). Because the paper is transparent, the dot just below them in the result (at 0,3) remains visible, as it can be seen through the transparent paper.

// Also notice that some dots can end up overlapping; in this case, the dots merge together and become a single dot.

// The second fold instruction is fold along x=5, which indicates this line:

// #.##.|#..#.
// #...#|.....
// .....|#...#
// #...#|.....
// .#.#.|#.###
// .....|.....
// .....|.....

// Because this is a vertical line, fold left:

// #####
// #...#
// #...#
// #...#
// #####
// .....
// .....

// The instructions made a square!

// The transparent paper is pretty big, so for now, focus on just completing the first fold. After the first fold in the example above, 17 dots are visible - dots that end up overlapping after the fold is completed count as a single dot.

// How many dots are visible after completing just the first fold instruction on your transparent paper?

#[macro_use]
extern crate ndarray;

use std::fmt;
use ndarray::prelude::*;



type PaperMarking = u8;
const PAPER_EMPTY: PaperMarking = 0;
const PAPER_DOT: PaperMarking = 1;




struct Point { x: u64, y: u64 }
enum FoldInstruction {
  FoldX(u64),
  FoldY(u64),
}



struct ChallengeInput {
  points: Vec<Point>,
  folds: Vec<FoldInstruction>,
}



struct Paper {
  points: Array2<PaperMarking>,
}

impl Paper {
  pub fn new_with_size(width: usize, height: usize) -> Self {
    Self {
      points: Array2::zeros((height, width))
    }
  }

  pub fn from_slice(slice: ArrayView<PaperMarking, Ix2>) -> Self {
    Self {
      points: slice.to_owned()
    }
  }
  
  pub fn fill(&mut self, x: usize, y: usize) {
    self.points[[y, x]] = PAPER_DOT;
  }

  fn get(&self, x: usize, y: usize) -> u8 {
    self.points[[y, x]]
  }

  pub fn count_visible(&self) -> u64 {
    let mut value = 0;
    for point in self.points.iter() {
      if *point == PAPER_DOT {
        value += 1;
      }
    }
    value
  }

  pub fn fold(&mut self, fold: &FoldInstruction) {
    match fold {
      FoldInstruction::FoldX(x_line) => {
        self.fold_x(*x_line as usize);
        // now resize the paper
        self.points = self.points.slice(s![
          0..,
          0..(*x_line as usize)
        ]).to_owned();
      },
      FoldInstruction::FoldY(y_line) => {
        self.fold_y(*y_line as usize);
        self.points = self.points.slice(s![
          0..(*y_line as usize),
          0..
        ]).to_owned();
      }
    }
  }

  fn fold_x(&mut self, x_line: usize) {
    for y in 0usize..self.points.shape()[0] {
      for x in (x_line+1usize)..self.points.shape()[1] {
        let value = self.get(x, y);
        if value == PAPER_EMPTY {
          continue;
        }
        let new_x = mirror_coordinate(x, x_line);
        
        self.fill(new_x, y);
      }
    }
  }

  fn fold_y(&mut self, y_line: usize) {
    for y in (y_line+1usize)..self.points.shape()[0] {
      for x in 0usize..self.points.shape()[1] {
        let value = self.get(x, y);
        if value == PAPER_EMPTY {
          continue;
        }
        let new_y = mirror_coordinate(y, y_line);
        
        self.fill(x, new_y);
      }
    }
  }
}


impl fmt::Display for Paper {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // write the cols
    for (y, row) in self.points.outer_iter().enumerate() {
      write!(f, "{:>4}   ", y)?;
      for col in row.outer_iter() {
        let inner = col.into_scalar();
        if *inner == 0u8 {
          write!(f, ".")?;
        } else if *inner == 1u8 {
          write!(f, "#")?;
        } else {
          write!(f, "?")?;
        }
      }
      write!(f, "\n")?;
    }
    Ok(())
  }
}






fn mirror_coordinate(coord: usize, mirroring_line: usize) -> usize {
  if coord > mirroring_line {
    mirroring_line - (coord - mirroring_line) 
  } else if coord < mirroring_line {
    coord
  } else {
    panic!("they said coord == mirroring_line cannot exist");
  }
}


#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn mirroring_works() {
      assert_eq!(
        mirror_coordinate(0, 5),
        0
      );
      assert_eq!(
        mirror_coordinate(1, 5),
        1
      );
      assert_eq!(
        mirror_coordinate(2, 5),
        2
      );
      assert_eq!(
        mirror_coordinate(3, 5),
        3
      );
      assert_eq!(
        mirror_coordinate(4, 5),
        4
      );
      assert_eq!(
        mirror_coordinate(6, 5),
        4
      );
      assert_eq!(
        mirror_coordinate(7, 5),
        3
      );
      assert_eq!(
        mirror_coordinate(8, 5),
        2
      );
      assert_eq!(
        mirror_coordinate(9, 5),
        1
      );
      assert_eq!(
        mirror_coordinate(10, 5),
        0
      );
  }

  #[test]
  fn example_fill_and_fold() {
    let mut paper = Paper::new_with_size(12, 15);

    paper.fill(6,10);
    paper.fill(0,14);
    paper.fill(9,10);
    paper.fill(0,3);
    paper.fill(10,4);
    paper.fill(4,11);
    paper.fill(6,0);
    paper.fill(6,12);
    paper.fill(4,1);
    paper.fill(0,13);
    paper.fill(10,12);
    paper.fill(3,4);
    paper.fill(3,0);
    paper.fill(8,4);
    paper.fill(1,10);
    paper.fill(2,14);
    paper.fill(8,10);
    paper.fill(9,0);

    println!("{}", paper);

    paper.fold(&FoldInstruction::FoldY(7));

    println!("{}", paper);

    assert_eq!(paper.count_visible(), 17);

    paper.fold(&FoldInstruction::FoldX(5));

    println!("{}", paper);
  }
}