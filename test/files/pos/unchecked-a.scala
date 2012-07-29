trait Y
trait Z extends Y
class X[+A <: Y]

object Test {
  def f1(x: X[_ <: Y]) = x match {
    case _: X[Any] => // looks a little funny; `Any` is outside the bounds for `A`
  }
  def f2(x: X[_ <: Y]) = x match {
    case _: X[Y]   => // looks better, let's allow this (too)
  }

  // NonLocalReturnControl[_] warnings
  def foo: Int = List(0).foldLeft(0){case _ => return 0}
}
