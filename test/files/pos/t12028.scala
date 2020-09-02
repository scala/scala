import scala.annotation.unchecked.uncheckedVariance

object Test {
  trait TypeClass[F[_]] {
    def action[A](foo: F[A], bar: F[A]): F[A]
  }

  class Syntax[F[+_], A](private val foo: F[A]) extends AnyVal {
    def action[F1[+x] >: F[x] @uncheckedVariance](bar: F1[A])(implicit tc: TypeClass[F1]): F1[A] =
      tc.action[A](foo, bar)
  }
}
