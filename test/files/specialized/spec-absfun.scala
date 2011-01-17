
/** Test inheritance. See #3085.
 *  Anonymous functions extend AbstractFunction1[SpecializedPair[Int], Unit]. The
 *  specialized type SpecializedPair$mcI$sp should not leak into the superclass because
 *  the definition of apply would vary covariantly, and erasure won't consider it an
 *  override of the abstract apply, leading to an AbstractMethodError at runtime.
 */

object Test {

  private val Max = 1000

  def main(args: Array[String]) {
    notSpecialized()
    specialized()
    println(runtime.BoxesRunTime.integerBoxCount)
  }

  def notSpecialized() {
    val pairs = for { i <- 1 to Max; j <- 1 to i } yield new Pair(i, j)
    val time0 = System.nanoTime
    pairs foreach { p => p.first * p.second }
    val time1 = System.nanoTime
//    println(time1 - time0)
  }

  def specialized() {
    val pairs = for { i <- 1 to Max; j <- 1 to i } yield new SpecializedPair(i, j)
    val time0 = System.nanoTime
    pairs foreach { p => p.first * p.second }
    val time1 = System.nanoTime
//    println(time1 - time0)
  }
}

class Pair[A](_first: A, _second: A) {
  def first = _first
  def second = _second
}

class SpecializedPair[@specialized(Int) A](_first: A, _second: A) {
  def first = _first
  def second = _second
}
