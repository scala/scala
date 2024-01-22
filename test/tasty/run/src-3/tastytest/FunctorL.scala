package tastytest

trait FunctorL[F[X] <: List[X]] {
  extension [A,B](fa: F[A]) def map(f: A => B): F[B]
}
