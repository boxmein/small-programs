use bake::*;

use tracing::debug;

fn main() {
    tracing_subscriber::fmt::init();

    let conf = read_bakefile().expect("failed to read bakefile");
    debug!("parsed bakefile to Config = {:?}", conf);
    run_config(conf);
}
