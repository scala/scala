package tester

object PatMatWarning {

  sealed trait X
  sealed trait Y

  def f(x: X) = x match {
    case _: Y => false
    case _    => true
  }

  class X1 extends X
  class Y1 extends Y
  class Z1 extends X with Y
}
