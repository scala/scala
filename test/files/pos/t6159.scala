//> using options -Werror
trait A {
  sealed abstract class X
  private class X1 extends X with X2 { }
  private trait X2 extends X
  sealed trait X3 extends X

  def f(x: X) = x match {
    case _: X1 => 0
  }
}
