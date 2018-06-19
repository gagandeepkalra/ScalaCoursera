import scala.util.control.NonFatal

/**
  * Try is a Monad with Left Unit Law Failing
  * Try(expr).flatMap(f) != f(expr)
  * L.H.S will never raise a NonFatal Exception, while R.H.S will
  *
  * @tparam T
  */
abstract class Try[+T] {

  def flatMap[U](f: T => Try[U]): Try[U] = this match {
    case Success(x) => f(x)
    case fail: Failure => fail
  }

  def map[U](f: T => U): Try[U] = this match {
    case Success(x) => Try(f(x))
    case fail: Failure => fail
  }
}

case class Success[T](x: T) extends Try[T]

case class Failure(ex: Exception) extends Try[Nothing]

object Try {

  def apply[T](expr: => T): Try[T] =
    try Success(expr)
    catch {
      case NonFatal(ex: Exception) => Failure(ex)
    }
}
