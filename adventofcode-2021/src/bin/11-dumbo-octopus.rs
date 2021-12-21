
use sdl2::pixels::Color;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::render::BlendMode;
use std::time::Duration;
use std::cell::UnsafeCell;
use typed_arena::Arena;

#[derive(Default, Debug, Clone)]
struct Octopus {
  value: i32,
  flashed: bool,
}

#[derive(Debug)]
struct Node<'a> {
    data: Octopus,
    edges: UnsafeCell<Vec<&'a Node<'a>>>,
}

struct Graph<'a> {
  arena: &'a Arena<Node<'a>>,
}

impl<'a> Graph<'a> {
  pub fn new(arena: &'a Arena<Node<'a>>) -> Self {
    Self {
      arena
    }
  }

  pub fn create_node(&mut self, value: i32) -> &'a Node<'a> {
    self.arena.alloc(Node::new(value))
  }
}

impl<'a> Node<'a> {
  pub fn new(
    value: i32
  ) -> Self {
    Self {
      data: Octopus {
        value,
        flashed: false,
      },
      edges: UnsafeCell::new(Vec::new()),
    }
  }

  pub fn incr(&mut self) {
    self.data.value += 1;
  }

  pub fn incr_cascade_flash(&mut self) {
    self.incr();

    if self.data.value > 9 {
      self.flash();
    }
  }

  pub fn flash_if_needed(&mut self) {
    if self.data.value > 9 {
      self.flash();
    }
  }

  
  fn flash(&mut self) {
    if !self.data.flashed {
      println!("flashed! neighbors: {}", (*self.edges.get()).len());
      self.data.flashed = true;
      self.data.value = 0;
      unsafe {
        for neighbor in &mut (*self.edges.get()) {
          neighbor.incr_cascade_flash();
          println!("flashed neighbor: {:?}", neighbor);
        }
      }
    }
  }

  pub fn begin_new_turn(&mut self) {
    self.data.flashed = false;
  }

  pub fn value(&self) -> i32 {
    self.data.value
  }

  pub fn add_neighbor(&mut self, neighbor: &'a Node<'a>) {
    (*self.edges.get()).push(neighbor);
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn octopus_counts_flashes() {
    let mut octo = Node::new(1);

    octo.incr();
    octo.incr();
    octo.incr();

    assert_eq!(octo.value(), 4);
  }

  #[test]
  fn octopus_flashes_if_above_9() {
    let mut octo = Node::new(8);

    octo.incr();
    octo.incr();
    octo.incr();

    octo.flash_if_needed();

    assert_eq!(octo.value(), 0);
  }

  #[test]
  fn octopus_doesnt_flash_if_9() {
    let mut octo = Node::new(8);

    octo.incr();

    octo.flash_if_needed();

    assert_eq!(octo.value(), 9);
  }

  #[test]
  fn octopus_flash_increments_neigbor() {
    let arena: Arena<Node> = Arena::new();

    let mut g = Graph::new(&arena);

    let mut octo = g.create_node(8);
    let mut octo2 = g.create_node(8);

    (*octo).add_neighbor(octo2);

    octo.incr();
    octo.incr();

    octo.flash_if_needed();

    assert_eq!(octo.value(), 0);
    assert_eq!(octo2.value(), 9);
  }
}

fn main() {
  let sdl_context = sdl2::init().unwrap();
  let video_subsystem = sdl_context.video().unwrap();

  let window = video_subsystem.window("Dumbo Octopus", 1000, 1000)
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

    canvas.present();
    ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
  }
}