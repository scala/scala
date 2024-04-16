//> using options -Xasync

import scala.tools.partest.async.OptionAwait._
import org.junit.Assert._

object Test {
  var counter = 0
  def ordered(i: Int, b: Boolean): Boolean = { assert(counter == i, (counter, i)); counter += 1; b }
  def f[T](t: T) = Option(t)
  def main(args: Array[String]): Unit = optionally {
      counter = 0; assert(!(ordered(0, false) && value(f(ordered(-1,  true)))))
      counter = 0; assert(!(ordered(0, false) && value(f(ordered(-1, false)))))
      counter = 0; assert( (ordered(0,  true) && value(f(ordered( 1,  true)))))
      counter = 0; assert(!(ordered(0,  true) && value(f(ordered( 1, false)))))
      counter = 0; assert( (ordered(0, false) || value(f(ordered( 1,  true)))))
      counter = 0; assert(!(ordered(0, false) || value(f(ordered( 1, false)))))
      counter = 0; assert( (ordered(0,  true) || value(f(ordered(-1, false)))))
      counter = 0; assert( (ordered(0,  true) || value(f(ordered(-1,  true)))))
      ()
    }
}
