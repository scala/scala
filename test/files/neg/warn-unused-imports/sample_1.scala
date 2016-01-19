
import language._

object Sample {
  trait X
  trait Y

  // import of the non-implicit should be unused
  object Implicits {
    def `int to X`(i: Int): X = null
    implicit def `int to Y`(i: Int): Y = null
    implicit def useless(i: Int): String = null
  }

  def f(x: X) = ???
  def g(y: Y) = ???
}
