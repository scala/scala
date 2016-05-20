package test

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](f: A => B, fa: F[A]): F[B]
}

object Functor {
  implicit def function[A]: Functor[({ type l[B] = A => B })#l] =
    new Functor[({ type l[B] = A => B })#l] {
      def map[C, B](cb: C => B, ac: A => C): A => B = cb compose ac
    }
}

object FunctorSyntax {
  implicit class FunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = F.map(f, fa)
  }
}

object Test {

  val f: Int => String = _.toString

  import FunctorSyntax._

  f.map((s: String) => s.reverse)
}
