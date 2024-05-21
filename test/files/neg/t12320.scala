//> using options -Werror -Xlint:infer-any,deprecation
//
trait T {
  def f = Option.empty[Int].contains("") || false
  def g(other: Option[Int]) = other.contains("") || false
  def any = Option.empty[Int].contains("")
}

trait `t9211 via 5898` {

  def f = List(1 -> "a", 2 -> "b", 3) map { p => val (a,b) = p; a + " -> " + b }
}

object review { def f(x: String) = 0; def f[T >: String](x: T) = 1 }

trait review {
  def test = review.f(42)
}
