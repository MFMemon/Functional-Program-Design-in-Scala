package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    
    val delta = Signal.Var(0.0)
    Signal{

      delta() = math.pow(b.currentValue,2) - 4 * a.currentValue * c.currentValue
      a()
      b()
      c()
    }
    delta

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    
    val roots = Signal.Var(Set(0.0))

    Signal{
      val deltaVal = math.sqrt(delta.currentValue)
      val aVal = a.currentValue
      val bVal = b.currentValue
      val cVal = c.currentValue
      if deltaVal.isNaN() then
        roots() = Set()
      else
        def num(sign : Int) = bVal * (-1) + sign * (deltaVal)
        val den = 2 * aVal * cVal
        roots() = Set(num(1) / den, num(-1) / den)
      delta()
    }

    roots
