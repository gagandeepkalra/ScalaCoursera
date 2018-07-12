package FunctionalReactiveProgramming

class Signal[T](expr: => T) {

  import Signal._

  private var myExpr: () => T = _
  private var myVal: T = _
  private var observers: Set[Signal[_]] = Set()
  update(expr)

  protected def update(expr: => T): Unit = {
    myExpr = () => expr
    computeValue()
  }

  protected def computeValue(): Unit = {
    val newVal = caller.withValue(this)(myExpr())
    if (newVal != myVal) {
      myVal = newVal
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())
    }
  }

  def apply(): T = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "cyclic signal defination")
    myVal
  }
}

object Signal {
  private val caller = new StackableVariable[Signal[_]](NoSignal) //  Threadlocal- new DynamicVariable[Signal[_]](NoSignal) or use implicits

  def apply[T](expr: => T): Signal[T] = new Signal(expr)
}

object NoSignal extends Signal[Nothing](???) {
  override protected def computeValue(): Unit = ()
}


class Var[T](expr: => T) extends Signal[T](expr) {
  override def update(expr: => T): Unit = super.update(expr) // access modifier eased to public
}

object Var {
  def apply[T](expr: => T): Signal[T] = new Var(expr)
}
