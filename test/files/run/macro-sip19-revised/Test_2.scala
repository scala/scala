import Macros._

object Test extends App {
  def foo(x: Int, y: Int)(implicit loc: SourceLocation): Int = {
    println("hey, i've been called from %s".format(loc))
    if (x < y) foo(y, x)
    else if (y == 0) x
    else foo(x - y, y)
  }

  println(foo(4, 2))
}
