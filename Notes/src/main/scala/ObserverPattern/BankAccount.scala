package ObserverPattern

class BankAccount extends Publisher {
  private var balance: Double = 0

  def currentBalance: Double = balance

  def deposit(amount: Double): Unit = {
    if (amount > 0) {
      balance += amount
      publish() // tell the subscribers
    }
  }

  def withdraw(amount: Double): Unit = {
    if (0 <= amount && amount <= balance) {
      balance -= amount
      publish() // tell the subscribers
    } else throw new Error("Insufficient Balance")
  }
}
