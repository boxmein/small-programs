use pcap::{Device, Address};
use std::net::IpAddr;
use anyhow::{Context, Result, anyhow};
use crate::docker::{get_network_ids_for_docker_container, get_gateway_ip_for_docker_net};
use tracing::debug;

pub fn print_network_adapters() -> Result<()> {
  let devices = Device::list()?;

  for device in devices {
    println!("Network interface: {}", device.name);
  }
  Ok(())
}


pub fn get_adapter_for_interface(interface_name: &str) -> Result<Device> {
  Ok(
    iter_devices()?
      .find(|dev| dev.name == interface_name)
      .ok_or_else(|| anyhow!("did not find interface"))?
  )
}


pub async fn get_adapter_for_docker_network(network_name_or_id: &str) -> Result<Device> {
  let ip = get_gateway_ip_for_docker_net(network_name_or_id)
    .await
    .context("tried to look up adapter by docker network ID")?;
  get_adapter_by_gateway_ip(&ip)
}

pub async fn get_adapters_for_docker_container(container_name_or_id: &str) -> Result<Vec<Device>> {
  let networks = get_network_ids_for_docker_container(container_name_or_id).await?;

  debug!("container {:?} networks are {:?}", container_name_or_id, networks);

  let mut devices = vec![];

  for net in networks {
    devices.push(get_adapter_for_docker_network(&net).await?);
  }
  Ok(devices)
}

fn get_adapter_by_gateway_ip(gateway_ip: &IpAddr) -> Result<Device> {
  Ok(
    iter_devices()?
      .find(|device| any_address_is(&device.addresses, gateway_ip))
      .ok_or_else(|| anyhow!("did not find interface"))?
  )
}

fn iter_devices() -> Result<impl Iterator<Item = Device>> {
  Ok(Device::list().context("error device listing")?.into_iter())
}

fn any_address_is(addrs: &[Address], expected: &IpAddr) -> bool {
  addrs.iter()
    .any(|address| &address.addr == expected)
}
