import scala.remoting._
class A(y: Int) {
  var z = 2
  var bar = (x: Int) => x + y + z
  def foo(x: Int): Int = x + y + z
  bar = (x: Int) => x * y
  detach(bar)
}

object test extends App {
  val a = new A(1)
  println(a.bar(2))
}
