package tastytest

trait FunctorL[F[_] <: List[_]] { def (fa: F[A]) map[A,B](f: A => B): F[B] }