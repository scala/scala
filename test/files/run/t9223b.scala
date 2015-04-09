class X(x: => String) { def xx = x }
class Y(y: String) extends X(y) { def f = y }

object Test {
  def main(args: Array[String]): Unit = {
     assert(new Y("hi").f == "hi")
  }
}
