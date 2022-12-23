mod cli;
mod docker;
mod global_docker;
mod packets;
mod network_adapters;

use anyhow::Result;

pub use cli::Cli;
use global_docker::connect_to_docker;
use network_adapters::{get_adapter_for_interface, get_adapters_for_docker_container};
pub use network_adapters::print_network_adapters;
pub use packets::log_all_packets;
use clap::Parser;
use pcap::Device;
use tracing::debug;

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt::init();
    let args = Cli::parse();
    connect_to_docker(&args.docker_host);
    print_network_adapters()?;

    let adapters = get_adapters(&args).await?;
    debug!("listening on adapters: {:?}", adapters);

    log_all_packets(&adapters, 
        args.filtering.filter.as_deref(), 
        &args.output.output_path
    ).await?;

    Ok(())
}

async fn get_adapters(args: &Cli) -> Result<Vec<Device>> {
    if let Some(interface) = &args.filtering.interface {
        debug!("looking up interface {}", interface);
        Ok(vec![
            get_adapter_for_interface(&interface)?
        ])
    } else if args.filtering.container_ids.len() > 0 {
        debug!("looking up container IDs {:?}", &args.filtering.container_ids);
        let containers = &args.filtering.container_ids;
        let container = &containers[0];
        debug!("container ID: {}", container);
        Ok(get_adapters_for_docker_container(container).await?)
    } else {
        todo!();
    }
}