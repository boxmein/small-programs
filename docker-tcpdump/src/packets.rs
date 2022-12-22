use pcap::Device;
use std::path::Path;

pub fn log_all_packets(filter: &str, output_path: impl AsRef<Path>) {
  let mut capture = Device::lookup()
    .expect("device lookup")
    .expect("no device")
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