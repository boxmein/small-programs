
use std::time::Duration;
use std::io::{stdin, BufRead, stdout, Write};
use crossterm::{
  Result as CrosstermResult,
  ExecutableCommand,
  terminal::{
    EnterAlternateScreen,
    LeaveAlternateScreen,
    Clear,
    ClearType,
  },
  cursor::{
    MoveTo
  },
  style::{
    SetForegroundColor,
    Color,
    ResetColor,
  },
};
use std::fmt;
use std::thread;
use std::str::FromStr;

static mut TOTAL_FLASHES: i32 = 0;

fn count_flash() {
  unsafe {
    TOTAL_FLASHES += 1;
  }
}

fn get_flashes() -> i32 {
  let flashes = unsafe { TOTAL_FLASHES };
  return flashes;
}

#[derive(Default, Debug, Clone)]
struct Octopus {
  pub value: i32,
  pub flashed: bool,
}

impl Octopus {
  pub fn new(value: i32) -> Self {
    Self {
      value,
      flashed: false,
    }
  }

  pub fn step(&mut self) {
    self.value += 1;
  }

  pub fn flash(&mut self) {
    count_flash();
    self.flashed = true;
    self.value = 0;
  }
}

impl std::fmt::Display for Octopus {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.flashed {
      write!(f, "{}{}{}",
        SetForegroundColor(Color::Magenta),
        "0",
        ResetColor,
      )
    } else {
      write!(f, "{}", self.value)
    }
  }
}

#[derive(Clone)]
struct Octos(Vec<Vec<Octopus>>);

impl<'a> Octos {
  pub fn at(&'a mut self, x: usize, y: usize) -> &'a mut Octopus {
    &mut self.0[y][x]
  }
  pub fn has(&self, x: usize, y: usize) -> bool {
    self.0.len() > y &&
    self.0[y].len() > x
  }
}

impl std::fmt::Display for Octos {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for row in &self.0 {
      for col in row {
        write!(f, "{}", col)?;
      }
      write!(f, "\n")?;
    }
    Ok(())
  }
}

impl FromStr for Octos {
    type Err = &'static str;
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let mut f = Octos(Vec::new());

        let arr = value
                .split("\n")
                .filter(|x| x.len() > 0)
                .map(|x|x.to_owned())
                .collect::<Vec<String>>();

        for (y, row) in arr.iter().enumerate() {

          f.0.push(Vec::new());

          for (x, col) in row.chars().enumerate() {
            f.0[y].push(Octopus::new(
              col.to_digit(10).unwrap().try_into().unwrap(),
            ));
          }
        } 

        Ok(f)
    }
}

fn octovec(width: usize, height: usize) -> Octos {
  // https://stackoverflow.com/a/36376568/2278637
  // Base 1d array
  let mut grid_raw: Vec<Octopus> = vec![Octopus::default(); width * height];

  // Vector of 'width' elements slices
  let octos = grid_raw
    .as_mut_slice()
    .chunks_mut(width)
    .map(|chunk| Vec::from(chunk))
    .collect::<Vec<Vec<Octopus>>>();

  Octos(octos)
}

fn run_increment(data: Octos) -> Octos {
  let mut new_octos = data.clone();
  
  // Step 1: each octopus energy level increases by 1.
  for row in &mut new_octos.0 {
    for col in row {
      col.step();
    }
  }

  new_octos
}

fn flash_point(data: &mut Octos, center: (usize, usize)) {
  let mut octo = data.at(center.0, center.1);
  octo.step();
  if octo.value > 9 && !octo.flashed {
    octo.flash();
    for dy in -1i32..2 {
      for dx in -1i32..2 {
        let xx: i32 = center.0 as i32 + dx;
        let yy: i32 = center.1 as i32 + dy;

        if data.has(xx as usize, yy as usize) &&
          !data.at(xx as usize, yy as usize).flashed {
          flash_point(data, (xx as usize, yy as usize));
        }
      }
    }
  }
}

fn run_flash(data: Octos) -> Octos {
  let mut new_octos = data.clone();


  for y in 0..new_octos.0.len() {
    let row_size = new_octos.0[y].len();
    for x in 0..row_size {
      if new_octos.at(x, y).value > 9 {
        flash_point(&mut new_octos, (x, y));
      }
    }
  }

  new_octos
}

fn reset_flashed(data: Octos) -> Octos {
  let mut new_octos = data.clone();

  for mut row in &mut new_octos.0 {
    for mut col in row {
      if col.flashed {
        col.flashed = false;
      }
    }
  }

  new_octos
}

fn tick_upto_reset(data: Octos) -> Octos {
  let step_1 = run_increment(data);
  run_flash(step_1)
}

fn finish_tick(data: Octos) -> Octos {
  reset_flashed(data)
}

fn main() -> CrosstermResult<()> {

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

  let mut data = s.parse::<Octos>().expect("failed to parse octos");

  // let mut data = octovec(10, 10);
  let mut stdout = stdout();

  stdout.execute(EnterAlternateScreen)?;
  stdout.execute(Clear(ClearType::All));

  let mut generation = 0;

  loop {
    generation += 1;
    data = tick_upto_reset(data);

    stdout.execute(MoveTo(1, 1))?;
    write!(stdout, "Generation: {}", generation);

    stdout.execute(MoveTo(1, 2))?;
    write!(stdout, "Flashes: {}", get_flashes());

    stdout.execute(MoveTo(1, 4))?;
    write!(stdout, "{}", data);

    data = finish_tick(data);
    thread::sleep(Duration::from_millis(100));
  }

  stdout.execute(LeaveAlternateScreen)?;
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test] 
  fn floor_from_str() {
  }
}