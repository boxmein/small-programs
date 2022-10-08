use crate::files::{get_max_mtime_glob, get_mtime};
use crate::model::Input;
use crate::traits::LastModifiedTimeable;
use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::time::SystemTime;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Output {
    File(String),
    Glob(String),
}

impl Output {
    pub fn provides_for(&self, input: &Input) -> Result<bool> {
        input.is_provided_by(&self)
    }
}

impl LastModifiedTimeable for Output {
    fn get_last_modified_timestamp(&self) -> Result<SystemTime> {
        match &self {
            &Output::File(ref filename) => get_mtime(filename),
            &Output::Glob(ref pattern) => get_max_mtime_glob(pattern),
        }
    }
}
