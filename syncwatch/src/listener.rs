use crossbeam_channel::Sender;
use notify::{RecursiveMode, Result, Watcher};
use std::path::Path;

pub struct Listener(Box<dyn Watcher>);

pub fn watch_directory_recursive(path: &Path, tx: Sender<notify::Event>) -> Result<Listener> {
    let mut watcher = notify::recommended_watcher(move |res| {
        if let Ok(event) = res {
            tx.send(event).expect("Did not get to send it");
        } else {
            println!("Error: {:?}", res);
        }
    })?;

    watcher.watch(Path::new(&path), RecursiveMode::Recursive)?;

    // watcher.configure(Config::PreciseEvents(true))?;

    Ok(Listener(Box::new(watcher)))
}
