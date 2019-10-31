package tastytest

import scala.runtime.ScalaRunTime

object TestPair extends Suite("TestPair") {

  test(assert(productString(Pair(true, 3)) === "Pair(true,3)"))
  test(assert(Pair(true, 3) match { case Pair(a, b) => a === true && b === 3 }))
  test(assert(Pair.fromProduct((true, 3)) == Pair(true, 3)))
  // test(assert(Pair.fromProduct((true, 3)) === (Pair(true, 3): Pair[_,_]))  ==> can't prove the types)

  def productString(p: Product) = ScalaRunTime._toString(p)

}
