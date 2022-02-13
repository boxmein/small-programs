import logging

from .repository import Repository

log = logging.getLogger("identity_service")

#
# Login system
#

class IdToken:
  id: str
  def __init__(self, user_id: str):
    assert type(user_id) == str
    self.id = user_id

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

