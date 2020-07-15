package tastytest

import scala.runtime.ScalaRunTime

object TestBox extends Suite("TestBox") {

  test("productString(Box(true)) is Box(true)") {
    assert(productString(Box(true)) === "Box(true)")
  }

  def productString(p: Product) = ScalaRunTime._toString(p)
}
