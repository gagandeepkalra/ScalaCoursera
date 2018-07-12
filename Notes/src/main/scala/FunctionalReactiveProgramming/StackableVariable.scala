package FunctionalReactiveProgramming

class StackableVariable[T](init: T) {
  private var values: List[T] = List(init)

  def value: T = values.head

  def withValue[R](newValue: T)(op: => R): R = {
    values = newValue :: values // push
    try
      op
    finally
      values = values.tail // pop
  }
}
