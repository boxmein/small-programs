from .repository import Repository


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
