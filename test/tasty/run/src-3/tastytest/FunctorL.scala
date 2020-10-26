package tastytest

trait FunctorL[F[_] <: List[_]] {
  extension [A,B](fa: F[A]) def map(f: A => B): F[B]
}
