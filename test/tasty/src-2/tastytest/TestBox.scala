package tastytest

import scala.runtime.ScalaRunTime

object TestBox {

  def test1 = assert(productString(Box(true)) === "Box(true)")

  def productString(p: Product) = ScalaRunTime._toString(p)

  def main(args: Array[String]): Unit = {
    test1
    println("Suite passed!")
  }
}