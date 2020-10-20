package tastytest

import Logarithms._

object TestLogarithms extends Suite("TestLogarithms") {

  val Some(l1) = Logarithm.of(2)
  val Some(l2) = Logarithm.of(3)

  test(assert((l1 + l2).toDouble == 4.999999999999999))
  test(assert((l1 * l2).toDouble == 6.0))

}
