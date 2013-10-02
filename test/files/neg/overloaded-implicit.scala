object Test {
  implicit def imp1[T](x: List[T]): Map[T, T] = Map()
  implicit def imp1[T](x: Set[T]): Map[T, T] = Map()

  def f[T <% Map[Int, Int]](x: T): Double = 1.0d

  // not parameterized, no warning
  implicit def imp2(x: List[Int]): String = "a"
  implicit def imp2(x: Set[Int]): String = "b"

  def g[T <% String](x: T): Double = 2.0d

  def main(args: Array[String]): Unit = {
    // println(f(List(1)))
    println(g(List(1)))
  }
}
