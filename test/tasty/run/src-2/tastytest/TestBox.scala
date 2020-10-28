package tastytest

import scala.runtime.ScalaRunTime

object TestBox extends Suite("TestBox") {

  test("productString(Box(true)) is Box(true)") {
    assert(productString(Box(true)) === "Box(true)")
  }

  test("Box(23).a is 23") {
    assert(Box(23).a === 23)
  }

  def productString(p: Product) = ScalaRunTime._toString(p)
}
