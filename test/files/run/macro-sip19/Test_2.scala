import Macros._

object Test extends App {
  def foo(x: Int, y: Int)(implicit loc0: SourceLocation): Int = {
    var loc = loc0;
    {
      var loc0 = 0 // shadow loc0 to disambiguate with the implicit macro
      println("hey, i've been called from %s".format(loc))
      if (x < y) foo(y, x)
      else if (y == 0) x
      else foo(x - y, y)
    }
  }

  println(foo(4, 2))
}
