//> using options -Werror
// like test/files/pos/t6159.scala
// but with T2 not private
trait A {
  sealed abstract class X
  private class X1 extends X with X2 { }
  trait X2 extends X
  sealed trait X3 extends X

  def f(x: X) = x match {
    case _: X1 => 0
  }
}
