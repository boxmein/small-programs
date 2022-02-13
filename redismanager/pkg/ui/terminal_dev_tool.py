
import inquirer

from ..admin import AdminService
from ..identity import IdentityService, IdToken
from ..money import MoneyService
from ..repository import Repository
from ..servers import ServerService

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
  ctx.server.remove_server(ctx.id_token.id)

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
