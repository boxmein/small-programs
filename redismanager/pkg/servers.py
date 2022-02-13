from .container_manager import ContainerManager
from .repository import Repository


class ServerService:
  def __init__(self, db: Repository, cman: ContainerManager): 
    self.db = db
    self.cman = cman

  def can_add_1_server(self, user_id: str):
    quota = int(self.db.read_user_quota(user_id))
    current = int(self.db.count_servers(user_id))

    print(f"can_add_1_server: {current} < {quota}: {current < quota}")

    if current < quota:
      return True
    return False

  def add_server(self, user_id: str) -> str:
    if not self.can_add_1_server(user_id):
      return None

    container = self.cman.create_client_redis(
      user_id
    )
    if container is not None:
      self.db.increase_server_count(user_id)
      container_ip = container.id # TODO: ip
      return f"redis://{container_ip}:6379"
    return None
    
  def remove_server(self, user_id: str) -> str:
    if self.cman.delete_redis_container(user_id):
      self.db.decrease_server_count(user_id)

  def server_details(self, user_id: str) -> dict:
    container = self.cman.get_container_for(f"client_{user_id}")
    if container is None:
      return None 

    container_ip = container.id # TODO: ip
    cpu = container.attrs['HostConfig']['CpuShares']
    mem = container.attrs['HostConfig']['Memory']

    return {
      "connection_details": f"redis://{container_ip}:6379",
      "cpu": cpu,
      "memory": mem
    }
