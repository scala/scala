
// scalac: -Werror -Xlint:infer-any

class C {
  val b = collection.mutable.ListBuffer.empty[Int]
  def f(i: Int, t: Boolean) = if (t) b += i else 42
  def g[A](a: A): A = a
  def h(i: Int, t: Boolean) = g(if (t) b += i)
  def n(i: Int, t: Boolean) =
    t match {
      case true => b += i
      case _ => 42
    }
  def z = List(42, "hello")
}
