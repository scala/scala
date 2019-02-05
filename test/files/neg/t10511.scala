// scalac: -deprecation -Xfatal-warnings
object Test {
  val f = Ordering[Float]
  val d = Ordering[Double]

  val list = List(1.0, 2.0, 3.0)
  list.sorted
}
