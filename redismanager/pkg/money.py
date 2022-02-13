from .repository import Repository


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
