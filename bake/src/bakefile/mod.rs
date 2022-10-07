use crate::model::Config;
use anyhow::Result;
use std::{
    fmt::Debug,
    path::{Path, PathBuf},
};
use tracing::debug;

pub fn find_bakefile() -> Result<PathBuf> {
    Ok("./bakefile.yml".into())
}

pub fn from_path(path: &(impl AsRef<Path> + Debug)) -> Result<Config> {
    let s = std::fs::read_to_string(path)?;

    debug!("bakefile was read in from the path = {path:?}", path = path);
    Ok(serde_yaml::from_str(&s)?)
}
