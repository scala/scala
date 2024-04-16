//> using options -Xsource:3
//

trait X
trait Y

class Test[A, B <: A & AnyRef] {
  def foo[T >: A & Null <: A & AnyRef & Any](x: T & ""): "" & T = x

  val a: X & Y & AnyRef = new X with Y {}
  val b: (X & Y) & AnyRef = new X with Y {}
  val c: X & (Y & AnyRef) = new X with Y {}

  val d: X & Y = c match {
    case xy: (X & Y) => xy
  }
}
