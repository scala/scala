package tastytest

import Intersections._

object TestIntersections extends Suite("TestIntersections") {

  val AwithB: AwithB = new A with B {}
  val AandB: AandB = new A with B {}

  test(assert(AwithBPrinter.println(AwithB) == "AwithB"))
  test(assert(AandBPrinter.println(AandB) == "A & B"))

}
