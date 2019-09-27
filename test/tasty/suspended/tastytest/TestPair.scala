package tastytest

import scala.runtime.ScalaRunTime

/** Suspended until case classes can be unpickled
 */
object TestPair {

  def test1 = assert(productString(Pair(true, 3)) === "Pair(true,3)")

  def productString(p: Product) = ScalaRunTime._toString(p)

  def main(args: Array[String]): Unit = {
    test1
    println("Suite passed!")
  }
}