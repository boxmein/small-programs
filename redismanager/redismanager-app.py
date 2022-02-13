#!/usr/bin/env python3 
# encoding: utf-8

import json
from typing import Container

import docker
import inquirer
import redis

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


#
# Login system
#



class IdToken:
  id: str
  def __init__(self, user_id: str):
    assert type(user_id) == str
    self.id = user_id

#
# Repository / Storage Layer
#

def get_repository_hostport():
  return 'localhost', 6379, 0

class Repository:
  def __init__(self, host, port, dbnum):
    self.client = redis.Redis(host=host, port=port, db=dbnum)

  #
  # User management
  #

  def create_account(self, username: str, password: str) -> str:
    new_id = self.client.incr("accounts")
    self.client.sadd("all_accounts", new_id)
    self.client.set(f"user_id_for:{username}", new_id)
    self.client.set(f"password_for:{username}", password)

    return new_id

  def is_user(self, username: str) -> bool:
    return self.client.get(f"user_id_for:{username}") is not None
  
  def check_password(self, username: str, password: str) -> bool:
    pw = str(self.client.get(f"password_for:{username}"), 'utf8')
    print(f"{pw} == {password}")
    return pw == password

  def list_accounts(self):
    return self.client.smembers("all_accounts")

  def delete_account(self, user_id):
    self.client.srem("all_accounts", user_id)

  def get_account_by_name(self, name) -> str:
    return str(self.client.get(f"user_id_for:{name}"), 'utf8')

  #
  # Quota tracking
  #

  def read_user_quota(self, user_id: str):
    return self.client.get(f"account:{user_id}:quota")

  def set_user_quota(self, user_id: str, quota: str): 
    return self.client.set(f"account:{user_id}:quota", quota)

  def increase_server_count(self, user_id: str):
    self.client.incr(f"account:{user_id}:servers")

  def decrease_server_count(self, user_id: str):
    self.client.decr(f"account:{user_id}:servers")

  def count_servers(self, user_id: str):
    return self.client.get(f"account:{user_id}:servers")

  #
  # invoices
  #

  def add_to_ltv(self, user_id: str, amount: float):
    self.client.incrbyfloat(f"account:{user_id}:ltv", amount)
  
  def get_ltv(self, user_id: str): 
    return self.client.get(f"account:{user_id}:ltv")

  def create_invoice(self, user_id: str, invoice_amount: float, month: str, other_data: dict):
    self.client.hset(f"account:{user_id}:invoice:{month}", "amount", invoice_amount)
    for key, value in other_data.items():
      self.client.hset(f"account:{user_id}:invoice:{month}", key, value)
    self.client.sadd(f"account:{user_id}:unpaid_invoices", month)

  def get_invoice_for_month(self, user_id: str, month: str):
    return self.client.get(f"account:{user_id}:invoice:{month}")

  def account_has_unpaid_invoices(self, user_id: str) -> bool:
    return self.client.scard(f"account:{user_id}:unpaid_invoices") > 0

  def get_unpaid_invoices(self, user_id: str) -> set[bytes]:
    return self.client.smembers(f"account:{user_id}:unpaid_invoices")

#
# Service layer
#

class ServerService:
  def __init__(self, db: Repository, cman: ContainerManager): 
    self.db = db
    self.cman = cman

  def can_add_1_server(self, user_id: str):
    quota = int(self.db.read_user_quota(user_id))
    current = int(self.db.count_servers(user_id))

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
    self.cman.delete_redis_container(user_id)

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

class MoneyService:
  def __init__(self, db: Repository): 
    self.db = db
  
  def create_monthly_invoice(self, user_id: str):
    count = self.db.count_servers(user_id)
    cash = 400 * count
    self.db.create_invoice(user_id, cash, "2019-01", {
      "count": count
    })
  
  def get_unpaid_invoices(self, user_id: str): 
    if not self.db.account_has_unpaid_invoices(user_id):
      return []
    return self.db.get_unpaid_invoices(user_id)

  def pay_invoice(self, user_id: str, invoice_id: str):
    detail = self.db.get_invoice_for_month(user_id, invoice_id)
    self.db.add_to_ltv(user_id, detail.cash)
    self.db.delete_invoice(user_id, invoice_id)
    


class AdminService:
  def __init__(self, db: Repository): 
    self.db = db

  def report(self): 
    for user_b in self.db.list_accounts():
      user = str(user_b, 'utf8')
      print(f"====================\nREPORT FOR {user}\n")
      print(f"Quota: {self.db.read_user_quota(user)}")
      print(f"Lifetime value: {self.db.get_ltv(user)}")
      print(f"Servers: {self.db.count_servers(user)}")
      print(f"Unpaid invoices: {self.db.account_has_unpaid_invoices(user)}")

class IdentityService:
  db: Repository 

  def __init__(self, db: Repository):
    self.db = db

  def create_account(self, name: str, password: str): 
    self.db.create_account(name, password)

  def is_user(self, name: str) -> bool:
    return self.db.is_user(name)

  def login(self, name: str, password: str) -> IdToken:
    if self.is_user(name):
      if self.db.check_password(name, password):
        accid = self.db.get_account_by_name(name)
        print(f"login success: {name} => idtoken {accid}")
        return IdToken(accid)
      else:
        print(f"Wrong password")
    else:
      print("No such user")


#
# UI
#

class RequestContext:
  id_token: str
  server: ServerService
  money: MoneyService
  admin: AdminService
  def __init__(self, id_token: IdToken, server: ServerService, money: MoneyService, admin: AdminService):
    self.id_token = id_token
    self.server = server
    self.money = money 
    self.admin = admin


def buy_redis_server(ctx: RequestContext):
  print("buy redis server")
  container = ctx.server.add_server(ctx.id_token.id)
  if container is not None:
    print(f"OK: created a Redis server for you ({container})")
  else:
    print(f"Warn: You ran out of quota! No more for you!")

def delete_redis_server(ctx: RequestContext):
  print("delete redis server")

def upscale_redis_server(ctx: RequestContext):
  print("upscale redis server")

def downscale_redis_server(ctx: RequestContext):
  print("downscale redis server")

def pay_invoice(ctx: RequestContext):
  unpaid = ctx.money.get_unpaid_invoices(ctx.id_token.id)

  if unpaid is None or len(unpaid) == 0:
    print("All good")
    return

  answer = inquirer.prompt("month", message="Which invoice?", options=unpaid)
  
  invoice = answer['month']

  invoice = ctx.money.get_invoice_for_month(ctx.id_token.id, invoice)
  if invoice is None:
    print("No such invoice")
    return 
  
  ctx.money.pay_invoice(ctx.id_token.id, invoice.id)


def create_report(ctx: RequestContext):
  print("====================================================================")
  print("\nREPORT FOR ALL ACCOUNTS\n")
  ctx.admin.report()
  print("====================================================================")

def list_servers(ctx: RequestContext):
  print("====================================================================")
  print("\nSERVERS\n")
  servers = ctx.server.server_details(ctx.id_token.id)
  if servers is None:
    print("No servers")
  else:
    print(servers)
  print("====================================================================")

def ui_loop(db: Repository, ctx: RequestContext):
  answers = inquirer.prompt([
    inquirer.List('action', message="?", choices=[
      "List Servers",
      "Buy redis server",
      "Pay invoice",
      "Delete redis server",
      "Upscale redis server",
      "Downscale redis server",
      "Admin Report"
    ])
  ])
  if answers['action'] == "Buy redis server":
    buy_redis_server(ctx)
  elif answers['action'] == "Pay invoice":
    pay_invoice(ctx)
  elif answers['action'] == "Delete redis server":
    delete_redis_server(ctx)
  elif answers['action'] == "Upscale redis server":
    upscale_redis_server(ctx)
  elif answers['action'] == "Downscale redis server":
    downscale_redis_server(ctx)
  elif answers['action'] == 'Admin Report':
    create_report(ctx)
  elif answers['action'] == "List Servers":
    list_servers(ctx)


def login_ui(identity: IdentityService) -> RequestContext:
  questions = [
    inquirer.Text('username', message="Username"),
    inquirer.Password('password', message="Password"),
  ]
  answers = inquirer.prompt(questions)

  idtok = identity.login(answers['username'], answers['password'])
  
  if idtok is None:
    answers2 = inquirer.prompt([
      inquirer.List("create", message="Create account?", choices=["Yes", "No"])
    ])

    if answers2['create'] == "Yes":
      identity.create_account(answers['username'], answers['password'])
      idtok = identity.login(answers['username'], answers['password'])


  print("logged in as", str(idtok))

  return idtok

if __name__ == '__main__':
  cman = ContainerManager()
  cman.assert_infra_exists()
  db = Repository(*get_repository_hostport())
  server = ServerService(db, cman)
  money = MoneyService(db)
  admin = AdminService(db)
  identity = IdentityService(db)
  idtok = login_ui(identity)
  ctx = RequestContext(idtok, server, money, admin)
  while True:
    ui_loop(db, ctx)
