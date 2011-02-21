object Test extends App {
  val v1: Array[Array[Int]] = Array(Array(1, 2), Array(3, 4))
  def f[T](w: Array[Array[T]]) {
    for (val r <- w) println(r.deep.toString)
  }
  f(v1)
}
