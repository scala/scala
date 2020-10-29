package tastytest

trait FunctorI[F[_ <: Int]] {
  extension [A <: Int, B <: Int](fa: F[A]) def map(f: A => B): F[B]
}
