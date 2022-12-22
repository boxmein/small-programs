mod cli;
mod docker;
mod global_docker;
mod packets;
mod network_adapters;

pub use cli::Cli;
pub use network_adapters::print_network_adapters;
pub use packets::log_all_packets;
use clap::Parser;

fn main() {
    tracing_subscriber::fmt::init();

    let args = Cli::parse();
    println!("Hello, world!");
    println!("Args: {:?}", args);

    print_network_adapters();

    log_all_packets(&args.filter, &args.output_path);
}
