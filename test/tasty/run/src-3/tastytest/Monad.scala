package tastytest

trait Monad[F[_]] extends Functor[F] {
  def pure[A](x: A): F[A]
  extension [A, B](fa: F[A]) {
    def flatMap(f: A => F[B]): F[B]
    def map (f: A => B) = flatMap(f `andThen` pure)
  }
}
