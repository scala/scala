package tastytest

import Logarithms._

object TestLogarithms {

  val Some(l1) = Logarithm.of(2)
  val Some(l2) = Logarithm.of(3)

  val l3 = l1 - l2 // error
  val l4 = l1 + l2 // ok (`+` is an extension method on Logarithm)
  val l5 = l1 * l2 // ok (`*` is an extension method on Logarithm)
  val d  = l1.toDouble // ok (`toDouble` is an extension method on Logarithm)

}
