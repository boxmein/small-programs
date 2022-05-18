use std::net::SocketAddr;
use std::path::Path;

pub struct Remote {
    hostport: SocketAddr,
    local_base: String,
    remote_base: String,
}

impl Remote {
    pub fn new(hostport: SocketAddr, local_base: String, remote_base: String) -> Self {
        Self {
            hostport,
            local_base,
            remote_base,
        }
    }

    pub fn from_string(s: String) -> Self {
        // ./syncwatch local_base host:remote_base
        // expecting host to be SSH accessible via rust ssh2-compatible methods
        unimplemented!();
    }

    pub fn apply_file_created(&self, path: &Path) {
        println!(
            "file was removed in local: {:?}, removing file in remote",
            path
        );
    }

    pub fn apply_file_or_dir_renamed(&self, path: &Path, new_path: &Path) {
        println!(
            "file renamed in local: {:?} => {:?}, renaming file in remote",
            path, new_path
        );
    }

    pub fn apply_file_contents_changed(&self, path: &Path) {
        println!(
            "file contents were changed in local: {:?}, updating file in remote",
            path
        );
    }

    pub fn apply_file_removed(&self, path: &Path) {
        println!(
            "file was removed in local: {:?}, removing file in remote",
            path
        );
    }

    pub fn apply_directory_created(&self, path: &Path) {
        println!(
            "directory created in local: {:?}, creating directory in remote",
            path
        );
    }

    pub fn apply_directory_removed(&self, path: &Path) {
        println!(
            "directory renamed in local: {:?}, renaming directory in remote",
            path
        );
    }
}
