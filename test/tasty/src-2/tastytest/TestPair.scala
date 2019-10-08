package tastytest

import scala.runtime.ScalaRunTime

/** Suspended until case classes can be unpickled
 */
object TestPair {

  def test1 = assert(productString(Pair(true, 3)) === "Pair(true,3)")
  def test2 = assert(Pair(true, 3) match { case Pair(a, b) => a === true && b === 3 })
  def test3 = assert(Pair.fromProduct((true, 3)) == Pair(true, 3))
  // def test3 = assert(Pair.fromProduct((true, 3)) === (Pair(true, 3): Pair[_,_]))  ==> can't compare the types

  def productString(p: Product) = ScalaRunTime._toString(p)

  def main(args: Array[String]): Unit = {
    test1
    test2
    test3
    println("Suite passed!")
  }
}
