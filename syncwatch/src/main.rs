mod listener;
mod remote;

use notify::{event::CreateKind, event::ModifyKind, EventKind};
use remote::Remote;
use std::{env::args, path::Path};

fn get_path_from_argv() -> String {
    let mut args = args();
    args.next();

    let argv1 = args.next().expect("pass a path on the command line!");

    argv1
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let (tx, rx) = crossbeam_channel::unbounded::<notify::Event>();

    let path_str = get_path_from_argv();
    let path = Path::new(&path_str);

    let _watcher = listener::watch_directory_recursive(path, tx)?;

    let remote = Remote::new(
        "127.0.0.1:2222".parse().unwrap(),
        "testdir1".to_string(),
        "~/testdir2".to_string(),
    );

    for ev in rx {
        match ev.kind {
            // notify::EventKind::Access(_) => todo!(),
            EventKind::Create(CreateKind::File) => remote.apply_file_created(&ev.paths[0]),
            EventKind::Create(CreateKind::Folder) => remote.apply_directory_created(&ev.paths[0]),
            EventKind::Modify(ModifyKind::Name(_)) => {
                println!("{:?}", ev.info());
                println!("{:?}", ev.flag());
                println!("{:?}", ev.attrs);
                remote.apply_file_or_dir_renamed(path, Path::new(""));
            }
            EventKind::Remove(_) => todo!(),
            EventKind::Any => todo!(),
            EventKind::Other => todo!(),
            _ => {}
        }
    }

    Ok(())
}
