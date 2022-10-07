use anyhow::Result;
use std::fmt::Display;
use std::path::Path;
use std::time::SystemTime;

use tracing::debug;

pub fn get_mtime<P: AsRef<Path> + Display>(filename: P) -> Result<SystemTime> {
    debug!("reading mtime for {}", filename);
    Ok(std::fs::metadata(filename)?.modified()?)
}
