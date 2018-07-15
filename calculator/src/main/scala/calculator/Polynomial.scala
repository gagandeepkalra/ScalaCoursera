package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal {
      val _b: Double = b()
      val _a: Double = a()
      val _c: Double = c()
      (_b * _b) - 4 * _a * _c
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val _a: Double = a()
      val _b: Double = b()
      val _c: Double = c()
      val _delta: Double = delta()

      _delta match {
        case zero if _delta < 0 => Set()
        case one if _delta == 0 => Set(-_b / (2 * _a))
        case two => Set((-_b + math.sqrt(_delta)) / (2 * _a), (-_b - math.sqrt(_delta)) / (2 * _a))
      }
    }
  }
}
