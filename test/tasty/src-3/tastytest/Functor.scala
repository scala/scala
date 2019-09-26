package tastytest

trait Functor[F[_]] { def (fa: F[A]) map[A,B](f: A => B): F[B] }