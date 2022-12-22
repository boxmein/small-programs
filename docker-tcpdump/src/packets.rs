use pcap::Device;
use std::path::Path;

use crate::network_adapters::get_adapter_for_interface;

pub fn log_all_packets(filter: &str, output_path: impl AsRef<Path>) {
  let mut capture = get_adapter_for_interface("docker0")
    .expect("missing docker0 network")
    .open()
    .expect("open");
  
  capture.filter(filter, true)
    .expect("filter is bad");

  let mut savefile = capture.savefile(output_path)
    .expect("savefile bad");

  while let Ok(packet) = capture.next_packet() {
    println!("{:?}", packet);
    savefile.write(&packet);
  }
}