import scala.tools.partest.Util.ArrayDeep

object Test extends App {
  val v1: Array[Array[Int]] = Array(Array(1, 2), Array(3, 4))
  def f[T](w: Array[Array[T]]): Unit = {
    for (r <- w) println(r.prettyArray.toString)
  }
  f(v1)
}
