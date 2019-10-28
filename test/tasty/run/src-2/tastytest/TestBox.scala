package tastytest

import scala.runtime.ScalaRunTime

object TestBox extends Suite("TestBox") {

  test("test1") { implicit ctx =>
    assert(productString(Box(true)) === "Box(true)")
  }

  def productString(p: Product) = ScalaRunTime._toString(p)
}
