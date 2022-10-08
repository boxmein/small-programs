use bake::*;

use tracing::debug;

fn main() {
    tracing_subscriber::fmt::init();

    debug!(
        "cwd is {}",
        std::env::current_dir()
            .expect("failed to get cwd")
            .display()
    );

    let conf = read_bakefile().expect("failed to read bakefile");
    debug!("parsed bakefile to Config = {:?}", conf);
    run_config(conf);
}
