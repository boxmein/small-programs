use crate::files::get_mtime;
use crate::model::Input;
use crate::traits::LastModifiedTimeable;
use anyhow::Result;
use std::time::SystemTime;

#[derive(Clone, Debug)]
pub enum Output {
    File(String),
}

impl Output {
    pub fn provides_for(&self, input: &Input) -> bool {
        input.is_provided_by(&self)
    }
}

impl LastModifiedTimeable for Output {
    fn get_last_modified_timestamp(&self) -> Result<SystemTime> {
        match &self {
            &Output::File(ref filename) => get_mtime(filename),
        }
    }
}
