
class D[T]

class C[F[_]](val i: Int)
object C {
  def apply[F[_]](implicit cf: C[F]): Int = cf.i

  implicit def c0[F[_]]: C[F] = new C[F](0)
  implicit def c1: C[D] = new C[D](1)
}

object Test extends App {
  assert(C[D] == 1) // Works in Dotty ...
}
