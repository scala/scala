package tastytest

import Logarithms._

object TestLogarithms extends Suite("TestLogarithms") {

  val Some(l1) = Logarithm.of(10)
  val Some(l2) = Logarithm.of(100)

  test(assert((l1 + l2).toDouble == 109.99999999999997))
  test(assert((l1 * l2).toDouble == 1000.0))

}
