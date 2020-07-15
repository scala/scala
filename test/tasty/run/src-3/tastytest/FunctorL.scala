package tastytest

trait FunctorL[F[_] <: List[_]] {
  def [A,B](fa: F[A]) map(f: A => B): F[B]
}
