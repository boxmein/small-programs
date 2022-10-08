use anyhow::{anyhow, Result};
use std::path::Path;
use std::time::SystemTime;

pub fn get_mtime<P: AsRef<Path>>(filename: P) -> Result<SystemTime> {
    Ok(std::fs::metadata(filename)?.modified()?)
}

pub fn get_max_mtime_glob(pattern: &str) -> Result<SystemTime> {
    let paths = glob::glob(pattern)?;
    let mtimes = paths.map(|x| get_mtime(x?));

    let result_vec_systemtime = mtimes.collect::<Result<Vec<SystemTime>>>()?;
    let max = result_vec_systemtime.iter().max();

    match max {
        Some(x) => Ok(x.clone()),
        None => Err(anyhow!("No max mtime found")),
    }
}

pub fn glob_matches_filename(pattern: &str, filename: &str) -> Result<bool> {
    let pat = glob::Pattern::new(pattern)?;
    Ok(pat.matches(filename))
}
