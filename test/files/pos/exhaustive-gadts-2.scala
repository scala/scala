sealed trait Nat[+T]
case class Zero() extends Nat[Nothing]
case class Succ[T]() extends Nat[T]

sealed trait Vect[+N <: Nat[_], +T]
case class VN[T]() extends Vect[Zero, T]
case class VC[T, N <: Nat[_]](x: T, xs: Vect[N, T]) extends Vect[Succ[N], T]

object Test {
  def foo[N <: Nat[_], A, B](v1: Vect[N, A], v2: Vect[N, B]) = (v1, v2) match {
    case (VN(), VN()) => 1
    case (VC(x, xs), VC(y, ys)) => 2
  }
}
