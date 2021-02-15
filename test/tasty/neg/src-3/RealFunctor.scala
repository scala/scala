package tastytest

// This `Functor` uses two type parameter lists in its `map` method.
// In this example, it may be safe to merge the type parameter lists.
trait RealFunctor[F[_]] {
  extension [A](fa: F[A]) def map[B](f: A => B): F[B]
}
