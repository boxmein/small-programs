import json

import docker

#
# Infrastructure (Docker)
#

REDISMANAGER_LABEL_PREFIX = "boxmein.redismanager"
REDISMANAGER_ADMINISTRATIVE_LABEL = REDISMANAGER_LABEL_PREFIX + ".admin"
REDISMANAGER_CLIENT_LABEL = REDISMANAGER_LABEL_PREFIX + ".client"
REDISMANAGER_CLIENT_ID_LABEL = REDISMANAGER_LABEL_PREFIX + ".client_id"

ADMIN_NET_NAME = 'redismanagera'
CLIENT_NET_NAME = 'redismanagerc'

REDIS_IMAGE = 'redis:latest'

class ContainerManager:

  def create_admin_net_if_needed(self):
    client = docker.from_env()
    try:
      client.networks.get(ADMIN_NET_NAME)
    except docker.errors.NotFound:
      print(f"Creating network {ADMIN_NET_NAME}")
      client.networks.create(
        ADMIN_NET_NAME, 
        driver="bridge",
        # internal=True,
        labels={
          REDISMANAGER_ADMINISTRATIVE_LABEL: "true"
        }
      )

  def create_client_net_if_needed(self):
    client = docker.from_env()
    try:
      client.networks.get(CLIENT_NET_NAME)
    except docker.errors.NotFound:
      print(f"Creating network {CLIENT_NET_NAME}")
      client.networks.create(
        CLIENT_NET_NAME, 
        driver="bridge",
        # internal=True,
        labels={
          REDISMANAGER_ADMINISTRATIVE_LABEL: "true"
        }
      )

  def create_redis_if_not_exists(self, label, network, container_name, **kwargs):
    client = docker.from_env()
    try:
      container = client.containers.get(container_name)
      if container.status != 'running':
        container.start()
      return None
    except docker.errors.NotFound:
      print(f"Creating redis {container_name}")
      container = client.containers.run(
        REDIS_IMAGE,
        detach=True,
        name=container_name,
        network=network,
        labels={
          label: "true",
        },
        **kwargs
      )
      print(f"{container.id} was created")
      return container

  def delete_redis_container(self, container_name):
    client = docker.from_env()
    try:
      container = client.containers.get(container_name)
      container.stop()
      container.remove()
    except docker.errors.NotFound:
      print(f"no container called f{container_name}")

  def create_admin_redis_if_needed(self):
    return self.create_redis_if_not_exists(
      REDISMANAGER_ADMINISTRATIVE_LABEL,
      ADMIN_NET_NAME,
      'admin',
      ports={
        6379: 6379,
      }
    )

  def create_client_redis(self, user_id: str):
    return self.create_redis_if_not_exists(
      REDISMANAGER_CLIENT_LABEL,
      CLIENT_NET_NAME,
      f'client_{user_id}'
    )

  def assert_infra_exists(self):
    self.create_admin_net_if_needed()
    self.create_client_net_if_needed()
    self.create_admin_redis_if_needed()

  def get_container_for(self, container_name: str):
    client = docker.from_env()
    try:
      container = client.containers.get(container_name)
      print(json.dumps(container.attrs))
      return container
    except docker.errors.NotFound:
      print(f"No container called {container_name}")
      return None
