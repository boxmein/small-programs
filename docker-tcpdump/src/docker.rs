use crate::global_docker::get_docker;

async fn get_gateway_ip_for_docker_net(network_id: &str) -> Option<String> {
  assert!(network_id.len() > 0);
  
  let docker = get_docker();
  let net = docker.networks().get(network_id).inspect().await.expect("Network read fail");
  let config = net.ipam.expect("IPAM key missing in docker network inspect").config.expect("Config missing inside IPAM inside docker network inspect");
  assert!(config.len() > 0);

  Some(config[0].gateway.clone().expect("gateway missing from IPAM config inside docker network inspect"))
}

pub fn get_interface_name_for_docker_bridge_network(network_id: &str) -> Option<String> {
  None
}

pub fn get_interface_id_for_docker_container(container_name_or_id: &str) -> Option<String> {
  None
}