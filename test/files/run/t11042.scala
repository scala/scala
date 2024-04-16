//> using options -Xlint:deprecation
object Test extends App {
  def f(xs: Array[Int]): Boolean = xs.isInstanceOf[scala.collection.immutable.Seq[_]]
  def g(xs: Int*): Boolean = xs.isInstanceOf[scala.collection.immutable.Seq[_]]

  println(f(Array(1, 2, 3)))
  println(g(Array(1, 2, 3): _*))
  println(g(1, 2, 3))
}
