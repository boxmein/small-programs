use crate::files::get_mtime;
use crate::model::Output;
use crate::traits::LastModifiedTimeable;
use anyhow::Result;
use std::time::SystemTime;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Input {
    File(String),
}

impl Input {
    pub fn is_provided_by(&self, output: &Output) -> bool {
        match (self, output) {
            (&Input::File(ref path), &Output::File(ref out_path)) => path == out_path,
        }
    }
}

impl LastModifiedTimeable for Input {
    fn get_last_modified_timestamp(&self) -> Result<SystemTime> {
        match &self {
            &Input::File(ref filename) => get_mtime(filename),
        }
    }
}
