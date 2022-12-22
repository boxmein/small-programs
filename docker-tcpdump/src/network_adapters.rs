use pcap::Device;

pub fn print_network_adapters() {
  let devices = Device::list().expect("failed to list devices");

  for device in devices {
    println!("Network device: {:?}", device);
  }
}

pub fn get_adapter_for_interface(interface_name: &str) -> Option<Device> {
  Device::list().expect("error device listing").into_iter().find(|dev| dev.name == interface_name)
}

pub fn get_adapter_for_docker_network(network_name_or_id: &str) -> Option<Device> {
  None
}

pub fn get_adapter_for_docker_container(container_name_or_id: &str) -> Option<Device> {
  None
}