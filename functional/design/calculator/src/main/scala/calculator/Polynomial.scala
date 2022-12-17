package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal({
      val bv = b()
      val av = a()
      val cv = c()
      bv*bv - 4*av*cv
    })

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal({
      if delta() < 0 then Set()
      else Set((-b() + Math.sqrt(delta())) / (2*a()), (-b() - Math.sqrt(delta())) / (2*a()))
    })
