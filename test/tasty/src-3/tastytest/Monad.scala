package tastytest

trait Monad[F[_]] extends Functor[F] {
  def pure[A](x: A): F[A]
  def (fa: F[A]) flatMap [A,B](f: A => F[B]): F[B]
  def (fa: F[A]) map [A,B] (f: A => B) = flatMap(fa)(f `andThen` pure)
}
