use anyhow::Result;
use std::fmt::Display;
use std::path::Path;
use std::time::SystemTime;

pub fn get_mtime<P: AsRef<Path> + Display>(filename: P) -> Result<SystemTime> {
    println!("reading mtime for {}", filename);
    Ok(std::fs::metadata(filename)?.modified()?)
}
