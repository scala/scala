package tastytest

import Intersections._

/** Effective minimisation of the ZIO issue with DefaultRuntime
 */
object TestIntersections {

  val AwithB: AwithB = new A with B {}
  val AandB: AandB = new A with B {}

  assert(AwithBPrinter.println(AwithB) == "AwithB")
  assert(AwithBPrinter.println(AandB) == "AwithB")

}
