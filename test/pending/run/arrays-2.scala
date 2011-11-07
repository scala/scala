//############################################################################
// Arrays 2
//############################################################################

//############################################################################

object Test extends Application {
  val a1 = Array(1, 2, 3)
  val a2 = Array(0, 7, 4, 2, 1, 3, 6, 5)
  val a3 = new Array(1, 2, 3) // ticket #193 (VerifyError)

  def _toString[A](a: Array[A]) = a.mkString("[", ",", "]")

  // slice: see file slices.scala
  println("a1=" + a1.reverse)
  println("a1=" + _toString(a1.reverse))
  println("a2=" + a2.filter(_ % 2 == 0))
  println("a2=" + _toString(a2.filter(_ % 2 == 0)))

  println("a2=" + _toString(a2))
  util.Sorting.stableSort(a2)
  println("a2=" + _toString(a2))
  
  println(a1 deepEquals a3)
}
