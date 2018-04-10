package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def findMatch(source: List[Char]): List[Char] = { // returns head of matching closed parentheses else empty List
      val c = source.head
      if (c == ')') source // true
      else {
        val tail = findMatch(source.tail)
        if (tail.isEmpty) List()
        else findMatch(tail.tail)
      }
    }

    @tailrec
    def evaluate(source: List[Char]): Boolean = {
      if (source.isEmpty) true
      else {
        if (source.head != '(' || source.tail.isEmpty) false
        else {
          val tail = findMatch(source.tail)
          if (tail.isEmpty) false
          else evaluate(tail.tail)
        }
      }
    }

    evaluate(chars.filter(c => c == '(' || c == ')'))
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def recur(m: Int, coins: List[Int]): Int = {
      m.signum match {
        case 0 => 1
        case -1 => 0
        case 1 => if (coins.nonEmpty) recur(m - coins.head, coins) + recur(m, coins.tail) else 0
      }
    }

    recur(money, coins)
  }
}
