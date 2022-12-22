mod lib;
use clap::Parser;

use crate::lib::*;

fn main() {
    tracing_subscriber::fmt::init();

    let args = Cli::parse();
    println!("Hello, world!");
    println!("Args: {:?}", args);

    print_network_adapters();

    log_all_packets(&args.filter, &args.output_path);
}