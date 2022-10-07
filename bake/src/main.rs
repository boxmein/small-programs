use bake::*;
use std::env::args;

fn main() {
    let filename = args().next().expect("usage: bake FILENAME");
    let conf = load_file(&filename);
    exec_graph(&conf);
}
