//! --- Day 5: Hydrothermal Venture ---

// You come across a field of hydrothermal vents on the ocean floor! These vents constantly produce large, opaque clouds, so it would be best to avoid them if possible.

// They tend to form in lines; the submarine helpfully produces a list of nearby lines of vents (your puzzle input) for you to review. For example:

// 0,9 -> 5,9
// 8,0 -> 0,8
// 9,4 -> 3,4
// 2,2 -> 2,1
// 7,0 -> 7,4
// 6,4 -> 2,0
// 0,9 -> 2,9
// 3,4 -> 1,4
// 0,0 -> 8,8
// 5,5 -> 8,2

// Each line of vents is given as a line segment in the format x1,y1 -> x2,y2 where x1,y1 are the coordinates of one end the line segment and x2,y2 are the coordinates of the other end. These line segments include the points at both ends. In other words:

//     An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
//     An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.

// For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2.

// So, the horizontal and vertical lines from the above list would produce the following diagram:

// .......1..
// ..1....1..
// ..1....1..
// .......1..
// .112111211
// ..........
// ..........
// ..........
// ..........
// 222111....

// In this diagram, the top left corner is 0,0 and the bottom right corner is 9,9. Each position is shown as the number of lines which cover that point or . if no line covers that point. The top-left pair of 1s, for example, comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.

// To avoid the most dangerous areas, you need to determine the number of points where at least two lines overlap. In the above example, this is anywhere in the diagram with a 2 or larger - a total of 5 points.

// Consider only horizontal and vertical lines. At how many points do at least two lines overlap?


use std::io::{stdin, BufRead};
use std::str::FromStr;
use std::collections::HashSet;
use std::time::Duration;

use sdl2::pixels::Color;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::render::BlendMode;

#[derive(Debug, PartialEq, Hash)]
enum Positivity {
  Positive,
  Negative
}

trait Positivable {
  fn sign(&self) -> Positivity;
}

impl Positivable for i32 {
  fn sign(&self) -> Positivity {
    if *self >= 0 {
      Positivity::Positive 
    } else {
      Positivity::Negative
    }
  }
}





/// Matrix of 2 x 2 values
/// [0 1 \
//   2 3]
struct Mat2x2(i32, i32, i32, i32);

impl Mat2x2 {
  pub fn determinant(&self) -> i32 {
    self.0 * self.3 - self.1 * self.2
  }
}





#[derive(Debug, Hash, Eq, PartialEq, Clone)]
struct Point { pub x: i32, pub y: i32 }

#[derive(Debug, Eq, PartialEq)]
struct PointParsingError;

impl FromStr for Point {
    type Err = PointParsingError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let coords: Vec<&str> = s.split(',')
                                 .collect();

        let x_fromstr = coords[0].parse::<i32>().map_err(|_| PointParsingError)?;
        let y_fromstr = coords[1].parse::<i32>().map_err(|_| PointParsingError)?;

        Ok(Point { x: x_fromstr, y: y_fromstr })
    }
}




#[derive(Debug, Hash, Eq, PartialEq, Clone)]
struct LineSegment(Point, Point);

impl LineSegment {
  // https://stackoverflow.com/a/3842157/2278637
  // NOTE: this approach will not work because there are two lines that
  // are parallel and on top of each other, causing more than 1 point!
  // NOTE 2: this approach will also not work because there are multiple lines
  // that intersect at the same location
  pub fn intersects(&self, other: &LineSegment) -> bool {
    let a = &self.0;
    let b = &self.1; 
    let c = &other.0;
    let d = &other.1;

    let left0 = Mat2x2(
      a.x - c.x,
      b.x - c.x,
      a.y - c.y,
      b.y - c.y
    ).determinant().sign();

    let right0 = Mat2x2(
      a.x - d.x,
      b.x - d.x,
      a.y - d.y,
      b.y - d.y
    ).determinant().sign();

    let left1 = Mat2x2(
      c.x - a.x,
      d.x - a.x,
      c.y - a.y,
      d.y - a.y
    ).determinant().sign();
    
    let right1 = Mat2x2(
      c.x - b.x,
      d.x - b.x,
      c.y - b.y,
      d.y - b.y
    ).determinant().sign();

    left0 != right0 &&
    left1 != right1
  }
}


type SetOfPairs = HashSet<(LineSegment, LineSegment)>;



struct Map {
  vertices: Vec<LineSegment>,
}

impl Map {
  pub fn new() -> Self {
    Map {
      vertices: vec![]
    }
  }
  pub fn add_vertex(&mut self, vertex: LineSegment) {
    println!("loaded vertex {:?}", vertex);
    self.vertices.push(vertex);
  }

  pub fn vertex_count(&self) -> usize {
    self.vertices.len()
  }

  pub fn intersections(&self) -> SetOfPairs {
    let mut result: SetOfPairs = HashSet::new();

    for v1 in &self.vertices {
      for v2 in &self.vertices {
        if v1.intersects(&v2) {
          let t1 = (v1.clone(), v2.clone());
          let t2 = (v2.clone(), v1.clone());

          if !result.contains(&t1) &&
             !result.contains(&t2) {
            result.insert(t1);
          }
        }
      }
    }

    result
  }

  pub fn iter_lines(&self) -> impl Iterator<Item = &LineSegment> {
    self.vertices.iter()
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
  let mut map = Map::new();
  
  for command in commands(stdin().lock()) {
    let command_parts = command.split(" ").collect::<Vec<&str>>();

    assert_eq!(command_parts.len(), 3);
    let first = command_parts[0];
    let last = command_parts[2];

    let first = first.parse::<Point>().unwrap();
    let last = last.parse::<Point>().unwrap();

    map.add_vertex(LineSegment(
      first,
      last
    ));
  }

  println!("loaded {} vertices", map.vertex_count());
  println!("result: {}", map.intersections().len());

  let sdl_context = sdl2::init().unwrap();
  let video_subsystem = sdl_context.video().unwrap();

  let window = video_subsystem.window("Hydrothermal Ventures", 1000, 1000)
    .position_centered()
    .build()
    .unwrap();

  let mut canvas = window.into_canvas().build().unwrap();

  canvas.set_draw_color(Color::RGB(0, 255, 255));
  canvas.set_blend_mode(BlendMode::Add);
  canvas.clear();
  canvas.present();

  let mut event_pump = sdl_context.event_pump().unwrap();
  'running: loop {
    canvas.set_draw_color(Color::RGB(0, 0, 0));

    canvas.clear();

    for event in event_pump.poll_iter() {
      match event {
        Event::Quit {..} |
        Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
            break 'running
        },
        _ => {}
      }
    }
    // The rest of the game loop goes here...

    canvas.set_draw_color(Color::RGB(60, 0, 0));


    for line in map.iter_lines() {
      canvas.draw_line(
        sdl2::rect::Point::new(line.0.x, line.0.y),
        sdl2::rect::Point::new(line.1.x, line.1.y)
      ).expect("failed to draw line");
    }

    canvas.present();
    ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
  }
}


#[cfg(test)]
mod tests {
  use super::{LineSegment, Point};
  #[test]
  fn determines_intersection_correctly() {
    // \/
    // /\
    let l1 = LineSegment(
      Point { x: -2, y: -2 },
      Point { x: 2, y: 2}
    );
    let l2 = LineSegment(
      Point { x: -2, y: 2 },
      Point { x: 2, y: -2}
    );

    assert_eq!(l1.intersects(&l2), true);
  }

  #[test]
  fn determines_non_intersection_correctly() {
    //  / |
    // /  |
    let l1 = LineSegment(
      Point { x: -2, y: -2 },
      Point { x: 2, y: 2}
    );
    let l2 = LineSegment(
      Point { x: 3, y: 2 },
      Point { x: 3, y: -2}
    );

    assert_eq!(l1.intersects(&l2), false);
  }

  #[test]
  fn shared_point_is_intersection() {
    //  / |
    // /  |
    let l1 = LineSegment(
      Point { x: -2, y: -2 },
      Point { x: 2, y: 2}
    );
    let l2 = LineSegment(
      Point { x: 3, y: 2 },
      Point { x: 2, y: 2}
    );

    assert_eq!(l1.intersects(&l2), false);
  }
}
