use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
  /// Which Docker networks to capture.
  #[arg(short, long)]
  pub networks: Vec<String>,

  /// Set a packet filter on the capture.
  /// Examples: 
  /// "tcp port 20"
  /// Reference: https://www.tcpdump.org/manpages/pcap-filter.7.html
  #[arg(short, long)]
  pub filter: String,

  /// File to write results to.
  #[arg(short, long)]
  pub output_path: String,
}