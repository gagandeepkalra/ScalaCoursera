package ObserverPattern

object Bank {

  def main(args: Array[String]): Unit = {
    val a, b = new BankAccount
    val c = new Consolidator(List(a, b))

    a deposit 10
    println(a currentBalance)
    println(c totalBalance)

    b deposit 25
    println(b currentBalance)
    println(c totalBalance)

  }

}
