use std::str::FromStr;
use std::collections::HashMap;
use std::convert::TryInto;
use std::io::{BufRead, stdin};

#[derive(Default, Debug, PartialEq, Eq, Clone)]
struct Location {
  x: i32,
  y: i32,
  value: i32,
}

impl Location {
  pub fn new(
    x: i32,
    y: i32,
    value: i32
  ) -> Self {
    Self {
      x,
      y,
      value
    }
  }

  pub fn value(&self) -> i32 {
    self.value
  }
}

#[derive(Default, PartialEq, Debug)]
struct Basin {
  pub locations: Vec<Location>
}

impl Basin {
  pub fn push(&mut self, x: &Location) {
    self.locations.push(x.clone());
  }

  pub fn contains(&self, x: &Location) -> bool {
    self.locations.contains(x)
  }

  pub fn len(&self) -> usize {
    self.locations.len()
  }
}

#[derive(Default, Debug, PartialEq)]
struct Floor {
  pub max_x: i32,
  pub max_y: i32,
  pub locations: Vec<Location>,
  pub y_x: HashMap<i32, HashMap<i32, Location>>,
}

impl FromStr for Floor {
    type Err = &'static str;
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let mut f = Floor::default();

        let arr = value
                .split("\n")
                .filter(|x| x.len() > 0)
                .map(|x|x.to_owned())
                .collect::<Vec<String>>();

        f.max_y = arr.len() as i32;
        f.max_x = arr[0].len() as i32;

        for (y, row) in arr.iter().enumerate() {

          f.y_x.insert(y as i32, HashMap::new());

          for (x, col) in row.chars().enumerate() {
            f.locations.push(Location::new(
              x as i32,
              y as i32,
              col.to_digit(10).unwrap().try_into().unwrap(),
            ));

            f.y_x.get_mut(&(y as i32))
              .unwrap()
              .insert(x as i32, Location::new(
                x as i32,
                y as i32, 
                col.to_digit(10).unwrap().try_into().unwrap(),
              ));
          }
        } 

        Ok(f)
    }
}

macro_rules! getget {
  ($t:expr, $x: expr, $y: expr) => {
    {
      $t
        .get(&($y))
        .map(|v| v.get(&($x)))
        .flatten()
        .map(|v| v.clone())
    }
  }
}

fn find_adjacencies(
  floor: &Floor,
  center: &Location
) -> Vec<Location> {
  // + represents the center, ABCD represent adjacents:
  //  A
  // B+C
  //  D
  let x = center.x;
  let y = center.y;

  let adjacencies: Vec<Option<Location>> = vec![
    getget!(floor.y_x, x, y - 1), // A
    getget!(floor.y_x, x - 1, y), // B
    getget!(floor.y_x, x + 1, y), // C
    getget!(floor.y_x, x, y + 1), // D
  ];

  adjacencies.into_iter()
    .filter(|x| x.is_some())
    .map(|x| x.expect("adjacency point was unwrapped unsuccessfully"))
    .collect::<Vec<Location>>()
}

fn find_local_minima(
  f: &Floor,
) -> Vec<Location> {
  let mut minima: Vec<Location> = vec![];
  for y in 0..f.max_y {
    for x in 0..f.max_x {
      let base = f.y_x[&y][&x].clone();

      let adjacencies = find_adjacencies(f, &base);

      let mut is_min = true;

      for adj in adjacencies {
        if base.value() >= adj.value() {
          is_min = false;
          break; 
        }
      }

      if is_min {
        minima.push(base.clone());
      }
    }
  }

  minima
}

fn expand_basin(
  floor: &Floor,
  basin: &mut Basin,
  loc: &Location,
) {
  let adjacencies = find_adjacencies(floor, loc);

  for adj in adjacencies {
    if adj.value == 9 {
      continue;
    }
    if basin.contains(&adj) {
      continue;
    }
    basin.push(&adj);
    expand_basin(floor, basin, &adj);
  }
}

fn find_basins(
  floor: &Floor,
  minima: &[Location],
) -> Vec<Basin> {
  let mut basins: Vec<Basin> = Vec::default();
  for location in minima {
    let mut basin = Basin::default();

    // basin includes the minimum
    basin.push(location);

    expand_basin(floor, &mut basin, &location);

    basins.push(basin);
  }

  basins
}

fn score(minima: &[Location]) -> i32 {
  let mut score = 0;
  for min in minima {
    score += 1 + min.value;
  }
  score
}

fn main() {
  let mut s: String = String::default();
  for line in stdin()
    .lock()
    .lines()
    .filter(|value| value.is_ok())
    .map(|value| value.expect("stdin failure")) {
    s += &line;
    s += "\n";
  }

  println!("loaded {} lines", s.split("\n").count());

  let floor = s.parse::<Floor>().expect("failed to parse floor");

  println!("floor is {} by {} tiles", floor.max_x, floor.max_y);

  let minima = find_local_minima(&floor);

  // Part 1
  println!("found {} local minima", minima.len());
  println!("task 1: {}", score(&minima));

  // Part 2
  let mut basins = find_basins(&floor, &minima);

  println!("found {} basins", basins.len());

  basins.sort_by(|a, b| {
    let a_len = a.len();
    let b_len = b.len();

    b_len.cmp(&a_len)
  });

  let mut result = 1;

  println!("basin lengths: ");
  for basin in basins.iter().take(3) {
    println!("    {}", basin.len());

    result *= basin.len();
  }

  println!("task 2: {}", result);
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn floor_from_str() {
    let s = r#"2199943210
3987894921
9856789892
8767896789
9899965678"#;
    
    let out = s.parse::<Floor>().expect("should've parsed");

    assert_eq!(out.locations[0], Location {
      x: 0,
      y: 0, 
      value: 2
    });

    assert_eq!(out.y_x[&0][&0], Location {
      x: 0,
      y: 0, 
      value: 2
    });

    assert_eq!(out.locations[49], Location {
      x: 9,
      y: 4, 
      value: 8
    });

    assert_eq!(out.y_x[&4][&9], Location {
      x: 9,
      y: 4, 
      value: 8
    });
  }

  #[test] 
  fn local_minima() {
    let s = r#"222
212
222"#;
    
    let floor = s.parse::<Floor>().expect("should've parsed");
    let minima = find_local_minima(&floor);

    assert_eq!(
      minima,
      vec![
        Location {
          x: 1,
          y: 1, 
          value: 1,
        }
      ]
    );
  }

  #[test] 
  fn local_minima_example() {
    let s = r#"2199943210
3987894921
9856789892
8767896789
9899965678"#;
    
    let floor = s.parse::<Floor>().expect("should've parsed");
    let minima = find_local_minima(&floor);

    assert_eq!(
      minima,
      vec![
        Location {
          x: 1,
          y: 0, 
          value: 1,
        },
        Location {
          x: 9,
          y: 0, 
          value: 0,
        },
        Location {
          x: 2,
          y: 2, 
          value: 5,
        },
        Location {
          x: 6,
          y: 4, 
          value: 5,
        },
      ]
    );
  }

  #[test]
  fn score_works() {
    let minima = &[
      Location { x: 0, y: 0, value: 2 },
      Location { x: 0, y: 0, value: 3 },
      Location { x: 0, y: 0, value: 4 },
      Location { x: 0, y: 0, value: 5 },
      Location { x: 0, y: 0, value: 6 },
      Location { x: 0, y: 0, value: 7 },
    ];

    assert_eq!(
      score(minima),
      1 + 2 + 1 + 3 + 1 + 4 + 1 + 5 + 1 + 6 + 1 + 7
    );
  }

  #[test]
  fn find_adjacencies_works() {
    let s = r#"2199943210
3987894921
9856789892
8767896789
9899965678"#;
    
    let floor = s.parse::<Floor>().expect("should've parsed");

    let adjs = find_adjacencies(&floor, &floor.y_x[&2][&2]);

    assert_eq!(adjs.len(), 4);

    assert_eq!(adjs[0], Location { x: 2, y: 1, value: 8 });
    assert_eq!(adjs[1], Location { x: 1, y: 2, value: 8 });
    assert_eq!(adjs[2], Location { x: 3, y: 2, value: 6 });
    assert_eq!(adjs[3], Location { x: 2, y: 3, value: 6 });
  }

  #[test]
  fn find_basins_works() {
    let s = r#"2199943210
3987894921
9856789892
8767896789
9899965678"#;
    
    let floor = s.parse::<Floor>().expect("should've parsed");

    // find basin for only 1 local minimum: [0, 0]
    let basins = find_basins(&floor, &[floor.locations[0].clone()]);

    assert_eq!(basins.len(), 1);

    let basin = &basins[0];

    assert_eq!(basin.len(), 3);
  }
}