use clap::{Parser, Args};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Cli {

  #[command(flatten)]
  pub output: Output, 

  #[command(flatten)]
  pub filtering: Filtering,

  #[arg(short, long, default_value = "unix:///var/run/docker.sock")]
  pub docker_host: String,
}

#[derive(Args, Debug)]
pub struct Filtering {
  /// Which Docker networks to capture.
  #[arg(short, long, 
      conflicts_with_all = ["container_ids", "interface"],
      required_unless_present_any = ["container_ids", "interface"]
  )]
  pub networks: Vec<String>,

  /// Capture all traffic for the given containers.
  #[arg(short, long,
      required_unless_present_any = ["networks", "interface"])]
  pub container_ids: Vec<String>,

  /// Which interface to capture
  #[arg(short, long,
      required_unless_present_any = ["networks", "container_ids"])]
  pub interface: Option<String>,

  /// Set a packet filter on the capture.
  /// Examples: 
  /// "tcp port 20"
  /// Reference: https://www.tcpdump.org/manpages/pcap-filter.7.html
  #[arg(short, long)]
  pub filter: Option<String>,
}

#[derive(Args, Debug)]
pub struct Output {
  /// File to write results to.
  #[arg(short, long)]
  pub output_path: String,
}