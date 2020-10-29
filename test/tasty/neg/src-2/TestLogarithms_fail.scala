package tastytest

import Logarithms._

object TestLogarithms {

  val Some(l1) = Logarithm.of(2)
  val Some(l2) = Logarithm.of(3)

  val l3: Double = l1 - l2 // error

}
