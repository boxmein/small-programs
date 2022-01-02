// --- Day 18: Snailfish ---

// You descend into the ocean trench and encounter some snailfish. They say they saw the sleigh keys! They'll even tell you which direction the keys went if you help one of the smaller snailfish with his math homework.

// Snailfish numbers aren't like regular numbers. Instead, every snailfish number is a pair - an ordered list of two elements. Each element of the pair can be either a regular number or another pair.

// Pairs are written as [x,y], where x and y are the elements within the pair. Here are some example snailfish numbers, one snailfish number per line:

// [1,2]
// [[1,2],3]
// [9,[8,7]]
// [[1,9],[8,5]]
// [[[[1,2],[3,4]],[[5,6],[7,8]]],9]
// [[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
// [[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]

// This snailfish homework is about addition. To add two snailfish numbers, form a pair from the left and right parameters of the addition operator. For example, [1,2] + [[3,4],5] becomes [[1,2],[[3,4],5]].

// There's only one problem: snailfish numbers must always be reduced, and the process of adding two snailfish numbers can result in snailfish numbers that need to be reduced.

// To reduce a snailfish number, you must repeatedly do the first action in this list that applies to the snailfish number:

//     If any pair is nested inside four pairs, the leftmost such pair explodes.
//     If any regular number is 10 or greater, the leftmost such regular number splits.

// Once no action in the above list applies, the snailfish number is reduced.

// During reduction, at most one action applies, after which the process returns to the top of the list of actions. For example, if split produces a pair that meets the explode criteria, that pair explodes before other splits occur.

// To explode a pair, the pair's left value is added to the first regular number to the left of the exploding pair (if any), and the pair's right value is added to the first regular number to the right of the exploding pair (if any). Exploding pairs will always consist of two regular numbers. Then, the entire exploding pair is replaced with the regular number 0.

// Here are some examples of a single explode action:

//     [[[[[9,8],1],2],3],4] becomes [[[[0,9],2],3],4] (the 9 has no regular number to its left, so it is not added to any regular number).
//     [7,[6,[5,[4,[3,2]]]]] becomes [7,[6,[5,[7,0]]]] (the 2 has no regular number to its right, and so it is not added to any regular number).
//     [[6,[5,[4,[3,2]]]],1] becomes [[6,[5,[7,0]]],3].
//     [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]] becomes [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] (the pair [3,2] is unaffected because the pair [7,3] is further to the left; [3,2] would explode on the next action).
//     [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]] becomes [[3,[2,[8,0]]],[9,[5,[7,0]]]].

// To split a regular number, replace it with a pair; the left element of the pair should be the regular number divided by two and rounded down, while the right element of the pair should be the regular number divided by two and rounded up. For example, 10 becomes [5,5], 11 becomes [5,6], 12 becomes [6,6], and so on.

// Here is the process of finding the reduced result of [[[[4,3],4],4],[7,[[8,4],9]]] + [1,1]:

// after addition: [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
// after explode:  [[[[0,7],4],[7,[[8,4],9]]],[1,1]]
// after explode:  [[[[0,7],4],[15,[0,13]]],[1,1]]
// after split:    [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
// after split:    [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
// after explode:  [[[[0,7],4],[[7,8],[6,0]]],[8,1]]

// Once no reduce actions apply, the snailfish number that remains is the actual result of the addition operation: [[[[0,7],4],[[7,8],[6,0]]],[8,1]].

// The homework assignment involves adding up a list of snailfish numbers (your puzzle input). The snailfish numbers are each listed on a separate line. Add the first snailfish number and the second, then add that result and the third, then add that result and the fourth, and so on until all numbers in the list have been used once.

// For example, the final sum of this list is [[[[1,1],[2,2]],[3,3]],[4,4]]:

// [1,1]
// [2,2]
// [3,3]
// [4,4]

// The final sum of this list is [[[[3,0],[5,3]],[4,4]],[5,5]]:

// [1,1]
// [2,2]
// [3,3]
// [4,4]
// [5,5]

// The final sum of this list is [[[[5,0],[7,4]],[5,5]],[6,6]]:

// [1,1]
// [2,2]
// [3,3]
// [4,4]
// [5,5]
// [6,6]

// Here's a slightly larger example:

// [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
// [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
// [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
// [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
// [7,[5,[[3,8],[1,4]]]]
// [[2,[2,2]],[8,[8,1]]]
// [2,9]
// [1,[[[9,3],9],[[9,0],[0,7]]]]
// [[[5,[7,4]],7],1]
// [[[[4,2],2],6],[8,7]]

// The final sum [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]] is found after adding up the above snailfish numbers:

//   [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
// + [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
// = [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]

//   [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
// + [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
// = [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]

//   [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]
// + [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
// = [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]

//   [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]
// + [7,[5,[[3,8],[1,4]]]]
// = [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]

//   [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]
// + [[2,[2,2]],[8,[8,1]]]
// = [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]

//   [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]
// + [2,9]
// = [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]

//   [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]
// + [1,[[[9,3],9],[[9,0],[0,7]]]]
// = [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]

//   [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]
// + [[[5,[7,4]],7],1]
// = [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]

//   [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]
// + [[[[4,2],2],6],[8,7]]
// = [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]

// To check whether it's the right answer, the snailfish teacher only checks the magnitude of the final sum. The magnitude of a pair is 3 times the magnitude of its left element plus 2 times the magnitude of its right element. The magnitude of a regular number is just that number.

// For example, the magnitude of [9,1] is 3*9 + 2*1 = 29; the magnitude of [1,9] is 3*1 + 2*9 = 21. Magnitude calculations are recursive: the magnitude of [[9,1],[1,9]] is 3*29 + 2*21 = 129.

// Here are a few more magnitude examples:

//     [[1,2],[[3,4],5]] becomes 143.
//     [[[[0,7],4],[[7,8],[6,0]]],[8,1]] becomes 1384.
//     [[[[1,1],[2,2]],[3,3]],[4,4]] becomes 445.
//     [[[[3,0],[5,3]],[4,4]],[5,5]] becomes 791.
//     [[[[5,0],[7,4]],[5,5]],[6,6]] becomes 1137.
//     [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]] becomes 3488.

// So, given this example homework assignment:

// [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
// [[[5,[2,8]],4],[5,[[9,9],0]]]
// [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
// [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
// [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
// [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
// [[[[5,4],[7,7]],8],[[8,3],8]]
// [[9,3],[[9,9],[6,[4,9]]]]
// [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
// [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]

// The final sum is:

// [[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]

// The magnitude of this final sum is 4140.

// Add up all of the snailfish numbers from the homework assignment in the order they appear. What is the magnitude of the final sum?

// To begin, get your puzzle input.

use std::ops::Add;
use std::rc::Rc;
use std::fmt;

use nom::{
  IResult,
};

#[derive(Clone, Debug, PartialEq, Eq)]
enum SnailfishNumber {
  Pair(Rc<SnailfishNumber>, Rc<SnailfishNumber>),
  Literal(i64),
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct SnailfishTop(Rc<SnailfishNumber>, Rc<SnailfishNumber>);

fn split(num: i64) -> SnailfishNumber {
  let div: i64 = num / 2;
  let modulo: i64 = num % 2;

  let left_value = div;
  let right_value = div + modulo;
  SnailfishNumber::Pair(
    Rc::new(SnailfishNumber::Literal(left_value)),
    Rc::new(SnailfishNumber::Literal(right_value)),
  )
}


fn sequence_recursive(
  acc: &mut Vec<Rc<SnailfishNumber>>,
  current: Rc<SnailfishNumber>,
) {
  match &*current {
    SnailfishNumber::Pair(left, right) => {
      sequence_recursive(acc, Rc::clone(left));
      sequence_recursive(acc, Rc::clone(right));
      return;
    },
    _ => {},
  }

  acc.push(current);
}


fn find_number_to_the_left_of(
  num: &SnailfishTop,
  center: Rc<SnailfishNumber>,
) -> Option<Rc<SnailfishNumber>> {
  let seq = num.sequence();

  for (i, item) in seq.iter().enumerate() {
    if are_refs_equal(item, &center) {
      if i == 0 {
        return None;
      } else {
        let r = &seq[i - 1];
        return Some(Rc::clone(r));
      }
    }
  }

  None
}

fn find_number_to_the_right_of(
  num: &SnailfishTop,
  center: Rc<SnailfishNumber>,
) -> Option<Rc<SnailfishNumber>> {
  let seq = num.sequence();

  for (i, item) in seq.iter().enumerate() {
    if are_refs_equal(item, &center) {
      if i == seq.len() - 1 {
        return None;
      } else {
        let r = &seq[i + 1];
        return Some(Rc::clone(r));
      }
    }
  }

  None
}


fn are_refs_equal(a: &Rc<SnailfishNumber>, b: &Rc<SnailfishNumber>) -> bool {
  Rc::ptr_eq(a, b)
}






impl SnailfishTop {
  pub fn reduce(&self) -> Self {
    let mut new = self.clone();

    loop {
      let (mut new, has_exploded) = new.attempt_explode();
      let (new, has_split) = new.attempt_split();

      if !has_exploded && !has_split {
        return new;
      }
    }
  }

  pub fn attempt_explode(&mut self) -> (Self, bool) {
    (
      self.clone(),
      false
    )
  }

  pub fn attempt_split(&mut self) -> (Self, bool) {
    let new = self.clone();
    let seq = new.sequence();

    let mut split_performed = false;

    for mut r in seq {
      match *r {
        SnailfishNumber::Literal(x) if x >= 10 => {
          let new_split= split(x);
          if let Some(m) = Rc::get_mut(&mut r) {
            *m = new_split;
          }
          split_performed = true;
        },
        _ => {}
      }
    }
    (
      new,
      split_performed
    )
  }

  pub fn sequence(&self) -> Vec<Rc<SnailfishNumber>> {
    let mut arr: Vec<Rc<SnailfishNumber>> = vec![];
    sequence_recursive(&mut arr, Rc::clone(&self.0));
    sequence_recursive(&mut arr, Rc::clone(&self.1));

    arr
  }

  pub fn magnitude(&self) -> i64 {
    self.0.magnitude() * 3 + self.1.magnitude() * 2
  }
}







fn parse_snailfish_literal(input: &str) -> IResult<&str, SnailfishNumber> {
  use nom::character::complete::*;
  let (input, num) = i64(input)?;
  Ok((input, SnailfishNumber::Literal(num)))
}

fn parse_snailfish_number(data: &str) -> IResult<&str, SnailfishNumber> {
  use nom::character::complete::*;
  use nom::branch::alt;
  let input: &str = &data;
  let (input, _) = char('[')(input)?;
  let (input, left) = alt((
    parse_snailfish_literal,
    parse_snailfish_number,
  ))(input)?;
  let (input, _) = char(',')(input)?;
  let (input, right) = alt((
    parse_snailfish_literal,
    parse_snailfish_number,
  ))(input)?;
  let (input, _) = char(']')(input)?;

  Ok(
    (
      input, 
      SnailfishNumber::Pair(
        Rc::new(left),
        Rc::new(right)
      )
    )
  )
}




impl SnailfishNumber {
  pub fn magnitude(&self) -> i64 {
    match self {
      SnailfishNumber::Pair(left, right) => {
        left.magnitude() * 3 + right.magnitude() * 2
      },
      SnailfishNumber::Literal(x) => *x,
    }
  }
}

impl Add for SnailfishNumber {
  type Output = Self;
  fn add(self, other: Self) -> Self {
    SnailfishNumber::Pair(
      Rc::new(self.clone()),
      Rc::new(other),
    )
  }
}

impl fmt::Display for SnailfishNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
          &SnailfishNumber::Literal(x) => write!(f, "{}", x),
          &SnailfishNumber::Pair(ref left, ref right) => {
            write!(f, "[{},{}]", left, right)
          },
        }
    }
}

fn main() {

}

#[cfg(test)]
mod tests {

  macro_rules! snail {
    () => {};
    
    ( [ [$($a:tt)*], [$($b:tt)*] ] ) => {
      SnailfishNumber::Pair(
        Rc::new(snail!( [ $($a)* ] )),
        Rc::new(snail!( [ $($b)* ] )),
      )
    };

    ( [ $a: expr, [$($b:tt)*] ] ) => {
      SnailfishNumber::Pair(
        Rc::new(SnailfishNumber::Literal($a)),
        Rc::new(snail!( [ $($b)* ] )),
      )
    };

    ( [ [$($a:tt)*], $b:expr ] ) => {
      SnailfishNumber::Pair(
        Rc::new(SnailfishNumber::Literal($a)),
        Rc::new(snail!( [ $($b)* ] )),
      )
    };
    
    ( [$a:expr, $b:expr] ) => {
      SnailfishNumber::Pair(
        Rc::new(SnailfishNumber::Literal($a)),
        Rc::new(SnailfishNumber::Literal($b)),
      )
    };
    
    ($a:expr) => {
      SnailfishNumber::Literal($a)
    };
  }

  macro_rules! snail_top {
    ($a:expr, $b:expr) => {
      SnailfishTop(Rc::new($a), Rc::new($b))
    }
  }

  use super::*;
  #[test]
  fn represents_snailfish_number() {
    let a = SnailfishNumber::Pair(
      Rc::new(SnailfishNumber::Literal(1)),
      Rc::new(SnailfishNumber::Literal(2)),
    );
    let _b = SnailfishNumber::Pair(
      Rc::new(a),
      Rc::new(SnailfishNumber::Literal(3)),
    );
  }

  #[test]
  fn split_works() {
    let inp = 12i64;
    let out = split(inp);

    assert_eq!(out, SnailfishNumber::Pair(
      Rc::new(SnailfishNumber::Literal(6)),
      Rc::new(SnailfishNumber::Literal(6)),
    ));
  }

  
  #[test]
  fn split_floor_and_ceil() {
    let inp = 13i64;
    let out = split(inp);

    assert_eq!(out, SnailfishNumber::Pair(
      Rc::new(SnailfishNumber::Literal(6)),
      Rc::new(SnailfishNumber::Literal(7)),
    ));
  }

  #[test]
  fn sequence_works() {
    let inp = SnailfishTop(
      Rc::new(SnailfishNumber::Literal(1)),
      Rc::new(SnailfishNumber::Literal(2)),
    );
    let out = inp.sequence();

    assert_eq!(out, vec![
      Rc::new(SnailfishNumber::Literal(1)),
      Rc::new(SnailfishNumber::Literal(2)),
    ]);
  }

  #[test]
  fn magnitude_works() {
    let inp = SnailfishTop(
      Rc::new(SnailfishNumber::Pair(
        Rc::new(SnailfishNumber::Literal(1)),
        Rc::new(SnailfishNumber::Literal(2)),
      )),
      Rc::new(SnailfishNumber::Pair(
        Rc::new(SnailfishNumber::Pair(
          Rc::new(SnailfishNumber::Literal(3)),
          Rc::new(SnailfishNumber::Literal(4)),
        )),
        Rc::new(SnailfishNumber::Literal(5)),
      )),
    );

    assert_eq!(inp.magnitude(), 143);
  }

  #[test]
  fn splits_snailfish_numeral_correctly() {
    let mut inp = snail_top! [ snail!(11), snail!(3) ];

    let (new_data, split_performed) = inp.attempt_split();
    
    assert_eq!(split_performed, true);

    assert_eq!(
      new_data,
      snail_top! [
        snail! { [6i64, 5] }, snail!(3)
      ],
    )
  }

  #[test]
  fn parser_works() {
    let inp = "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]";
    let (rest, out) = parse_snailfish_number(inp).unwrap();
    assert_eq!(
      out,
      SnailfishNumber::Pair(
        Rc::new(SnailfishNumber::Pair(
          Rc::new(SnailfishNumber::Literal(2)),
          Rc::new(SnailfishNumber::Pair(
            Rc::new(SnailfishNumber::Pair(
              Rc::new(SnailfishNumber::Literal(0)),
              Rc::new(SnailfishNumber::Literal(8)),
            )),
            Rc::new(SnailfishNumber::Pair(
              Rc::new(SnailfishNumber::Literal(3)),
              Rc::new(SnailfishNumber::Literal(4)),
            )),
          )),
        )),
        Rc::new(SnailfishNumber::Pair(
          Rc::new(SnailfishNumber::Pair(
            Rc::new(SnailfishNumber::Pair(
              Rc::new(SnailfishNumber::Literal(6)),
              Rc::new(SnailfishNumber::Literal(7)),
            )),
            Rc::new(SnailfishNumber::Literal(1)),
          )),
          Rc::new(SnailfishNumber::Pair(
            Rc::new(SnailfishNumber::Literal(7)),
            Rc::new(SnailfishNumber::Pair(
              Rc::new(SnailfishNumber::Literal(1)),
              Rc::new(SnailfishNumber::Literal(6)),
            )),
          )),
        )),
      ),
    );
  }

  #[test]
  fn formatter_works() {
    let sn = snail! [[
      [ 2, 3 ],
      [ 4, 5 ]
    ]];

    assert_eq!(
      format!("{}", sn),
      "[[2,3],[4,5]]"
    );
  }
}