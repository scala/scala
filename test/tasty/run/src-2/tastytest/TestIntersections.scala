package tastytest

import Intersections._

object TestIntersections extends Suite("TestIntersections") {

  val AwithB: AwithB = new A with B {}
  val AandB: AandB = new A with B {}
  val AandBandCandD: AandBandCandD = new A with B with C with D {}

  test(assert(AwithBPrinter.println(AwithB) == "AwithB"))
  test(assert(AandBPrinter.println(AandB) == "A & B"))
  test(assert(AandBandCandDPrinter.println(AandBandCandD) == "A & B & C & D"))

}
