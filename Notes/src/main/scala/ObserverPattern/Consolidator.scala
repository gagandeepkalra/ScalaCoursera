package ObserverPattern

/*
A Subscriber to maintain the total balance of a list of accounts
 */
class Consolidator(observed: List[BankAccount]) extends Subscriber {

  observed foreach (_.subscribe(this)) // subscribe to all the bank accounts

  private var total: Double = _
  compute()

  private def compute(): Unit = total = observed.map(_.currentBalance).sum

  override def handler(pub: Publisher): Unit = compute() // whenever balance in any account changes

  def totalBalance: Double = total
}
