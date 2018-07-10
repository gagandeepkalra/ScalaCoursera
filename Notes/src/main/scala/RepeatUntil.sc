def repeat(b: => Unit) = new {
  def until(cond: => Boolean): Unit = {
    b
    if (cond) until(cond)
  }
}

var i = 0

repeat {
  println(i)
  i += 1
} until (i < 5)



def recur(n: => Int): Stream[Int] = {
  n #:: recur(n + 1)
}

recur(10).take(10)
