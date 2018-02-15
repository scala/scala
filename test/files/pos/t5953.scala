trait CBF[-F, -A, +C]
trait GenTraversable[+A]
trait Traversable[+A] extends GenTraversable[A]
trait Vector[A] extends Traversable[A]
object Vector {
  implicit def cbf[A]: CBF[Vector[_], A, Vector[A]] = ???
}

package object foo {
  @inline implicit class TravOps[A, CC[A] <: GenTraversable[A]](val coll: CC[A]) extends AnyVal {
    def build[CC2[X]](implicit cbf: CBF[Nothing, A, CC2[A]]): CC2[A] = ???
  }
}

package foo {
  object Test {
    def f2[T](xs: Traversable[T]) = xs.build[Vector]
  }
}
