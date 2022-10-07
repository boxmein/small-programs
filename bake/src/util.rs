use crate::traits::LastModifiedTimeable;
use anyhow::{anyhow, Result};
use std::time::SystemTime;

pub fn get_max_mtime(items: &[impl LastModifiedTimeable]) -> Result<SystemTime> {
    let iterator = items.iter();

    let times = iterator
        .map(|x| x.get_last_modified_timestamp())
        .collect::<Result<Vec<SystemTime>>>()?;

    let max_time = times.iter().max();

    match max_time {
        Some(res) => Ok(*res),
        None => Err(anyhow!("failed to find a maximum")),
    }
}
