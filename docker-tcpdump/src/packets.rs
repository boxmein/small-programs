use anyhow::{Context, Result};
use pcap::Device;
use std::path::Path;

pub async fn log_all_packets(adapters: &[Device], filter: Option<&str>, output_path: impl AsRef<Path>) -> Result<()> {
  assert!(adapters.len() > 0);
  let adapter = adapters[0].clone();
  let mut capture = adapter
    .open()
    .context("tried to open a capture on the network")?;
  
  if let Some(filter) = filter {
    capture.filter(filter, true)
      .context("failed to apply a filter program")?;
  }

  let mut savefile = capture.savefile(output_path)
    .context("failed to open a PCAP save file")?;



  while let Ok(packet) = capture.next_packet().context("tried to get next packet") {
    let pkt = packet;
    println!("pkt is: {:?}", pkt);
    savefile.write(&pkt);
  }

  Ok(())
}
