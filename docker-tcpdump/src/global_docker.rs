use docker_api::Docker;
use once_cell::sync::OnceCell;

static INSTANCE: OnceCell<Docker> = OnceCell::new();

pub fn get_docker() -> &'static Docker {
  INSTANCE.get().expect("Docker is not initialized")
}

pub fn connect_to_docker(url: &str) {
  let docker = Docker::new(url).expect("failed to connect to docker");
  INSTANCE.set(docker).expect("failed to store docker in once_cell");
}