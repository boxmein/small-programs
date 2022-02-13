#!/usr/bin/env python3 
# encoding: utf-8

from pkg.admin import AdminService
from pkg.container_manager import ContainerManager
from pkg.identity import IdentityService
from pkg.money import MoneyService
from pkg.repository import Repository, get_repository_hostport
from pkg.servers import ServerService
from pkg.ui.terminal_dev_tool import RequestContext, login_ui, ui_loop

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
