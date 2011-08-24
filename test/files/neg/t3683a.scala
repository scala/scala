sealed trait Foo
sealed trait Bar extends Foo
sealed trait W[T >: Bar <: Foo]
case class X() extends W[Foo]
case class XX() extends W[Bar]
case class Y() extends W[Bar]
case class Z[T >: Bar <: Foo](
  z1: W[T]
) extends W[T]

object Main {
  // should warn for not including XX()
  def f1(w: W[Bar]): Int = {
    w match {
      // case XX() => 2
      case Y() => 1
      case Z(z) => f1(z)
    }
  }
}