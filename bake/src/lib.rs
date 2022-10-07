mod bakefile;
mod context;
mod files;
mod model;
mod run;
mod traits;
mod util;

use anyhow::Result;
use context::get_context;
use model::Config;
use tracing::debug;

pub fn run_config(config: Config) {
    let ctx = get_context();
    let result = run::run_config(config, &ctx);

    debug!("run finished, resulting in a RunResult = {:?}", result);
}

pub fn read_bakefile() -> Result<Config> {
    let path = bakefile::find_bakefile()?;
    bakefile::from_path(&path)
}
