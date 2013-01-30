trait Covariant[+A]
case class Invariant[A](xs: Array[A]) extends Covariant[A]

class Test {
  val arr = Array("abc")
  def f[A](v: Covariant[A]) /*inferred!*/ = v match { case Invariant(xs) => xs }
  f(Invariant(arr): Covariant[Any])(0) = Nil
}