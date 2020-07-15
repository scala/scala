package tastytest

import Logarithms._

object TestLogarithms extends Suite("TestLogarithms") {

  val Some(l1) = Logarithm.of(2)
  val Some(l2) = Logarithm.of(3)

  val l3: Double = l1 - l2 // currently opaque type aliases are transparent to Scala 2

  test(assert(logarithmOps.toDouble(logarithmOps.+(l1)(l2)) === 4.999999999999999))

}
