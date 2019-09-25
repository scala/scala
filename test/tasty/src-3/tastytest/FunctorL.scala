package tastytest

trait FunctorL[F[_] <: List[_]] { def map[A,B](fa: F[A])(f: A => B): F[B] }