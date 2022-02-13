import redis

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
