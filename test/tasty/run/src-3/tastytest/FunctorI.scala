package tastytest

trait FunctorI[F[_ <: Int]] {
  def (fa: F[A]) map[A <: Int, B <: Int](f: A => B): F[B]
}
