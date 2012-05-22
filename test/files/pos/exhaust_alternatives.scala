sealed abstract class X
sealed case class A(x: Boolean) extends X
case object B extends X

object Test {
  def test(x: X) = x match {
    case A(true) =>
    case A(false) | B =>
  }
}