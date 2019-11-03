package tastytest

trait FunctorI[F[_ <: Int]] {
  def [A <: Int, B <: Int](fa: F[A]) map(f: A => B): F[B]
}
