use std::net::IpAddr;
use anyhow::{Result, Context};
use tracing::debug;
use crate::global_docker::get_docker;

pub async fn get_gateway_ip_for_docker_net(network_id: &str) -> Result<IpAddr> {
  assert!(network_id.len() > 0);
  
  let docker = get_docker();
  let net = docker.networks()
  .get(network_id)
  .inspect()
  .await
  .context("Network read fail")?;

  let config = net.ipam
    .context("IPAM key missing in docker network inspect")?
    .config
    .context("Config missing inside IPAM inside docker network inspect")?;
  
    assert!(config.len() > 0);

  let gateway_ip = config[0]
    .gateway
    .clone()
    .context("gateway missing from IPAM config inside docker network inspect")?;

  debug!("network {} gateway IP is {}", network_id, gateway_ip);
  
  gateway_ip.parse().context("parse IpAddr")
}

pub async fn get_network_ids_for_docker_container(container_name_or_id: &str) -> Result<Vec<String>> {
  let docker = get_docker();
  let container = docker.containers().get(container_name_or_id).inspect().await.context("Network read fail")?;

  let networks = 
    container.network_settings
      .context("missing network settings")?
      .networks
      .context("missing networks list")?;

  debug!("container {:?} has {} networks", container_name_or_id, networks.len());
  
  networks
    .iter()
    .map(|(_, v)| v)
    .map(|v| v.network_id.clone().context("missing network ID"))
    .collect()
}