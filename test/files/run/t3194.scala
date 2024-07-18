//> using options -language:_
import scala.util.chaining._

class A(var x: Int)
class B(x: Int) extends A(x) {
  def update(v: Int): Unit = x = v
}

object Test extends App {
  new B(27).tap(_.update(42)).ensuring(_.x == 42)
}
