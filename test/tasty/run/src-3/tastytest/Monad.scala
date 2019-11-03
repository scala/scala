package tastytest

trait Monad[F[_]] extends Functor[F] {
  def pure[A](x: A): F[A]
  def [A,B](fa: F[A]) flatMap(f: A => F[B]): F[B]
  def [A,B](fa: F[A]) map (f: A => B) = flatMap(fa)(f `andThen` pure)
}
