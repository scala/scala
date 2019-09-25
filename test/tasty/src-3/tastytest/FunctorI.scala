package tastytest

trait FunctorI[F[_ <: Int]] { def map[A <: Int, B <: Int](fa: F[A])(f: A => B): F[B] }