use crate::files::{get_max_mtime_glob, get_mtime, glob_matches_filename};
use crate::model::Output;
use crate::traits::LastModifiedTimeable;
use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::time::SystemTime;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Input {
    File(String),
    Glob(String),
}

impl Input {
    pub fn is_provided_by(&self, output: &Output) -> Result<bool> {
        match (self, output) {
            (&Input::File(ref path), &Output::File(ref out_path)) => Ok(path == out_path),

            (&Input::Glob(ref pattern), &Output::File(ref filename)) => {
                glob_matches_filename(pattern, filename)
            }
            (&Input::File(ref filename), &Output::Glob(ref pattern)) => {
                glob_matches_filename(pattern, filename)
            }

            (&Input::Glob(ref pattern1), &Output::Glob(ref pattern2)) => Ok(pattern1 == pattern2),
        }
    }
}

impl LastModifiedTimeable for Input {
    fn get_last_modified_timestamp(&self) -> Result<SystemTime> {
        match &self {
            &Input::File(ref filename) => get_mtime(filename),
            &Input::Glob(ref pattern) => get_max_mtime_glob(pattern),
        }
    }
}
