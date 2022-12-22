use pcap::Device;

pub fn print_network_adapters() {
  let devices = Device::list().expect("failed to list devices");

  for device in devices {
    println!("Network device: {:?}", device);
  }
}
