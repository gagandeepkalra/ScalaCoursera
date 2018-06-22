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


