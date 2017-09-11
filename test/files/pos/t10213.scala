import scala.language.higherKinds

final case class Coproduct[F[_], G[_], A](run: Either[F[A], G[A]])

object Coproduct {

  sealed trait Builder {
    type Out[_]
  }

  sealed trait :++:[F[_], G[_]] extends Builder {
    type Out[A] = Coproduct[F, G, A]
  }

  sealed trait :+:[F[_], B <: Builder] extends Builder {
    type Out[A] = Coproduct[F, B#Out, A]
  }
}

trait Inject[F[_], H[_]] {
  def inj[A](fa: F[A]): H[A]
}

object Inject {
  import Coproduct._

  implicit def reflexiveInject[F[_]]: Inject[F, F] =
    new Inject[F, F] {
      def inj[A](fa: F[A]): F[A] = fa
    }

  implicit def injectLeft[F[_], G[_]]: Inject[F, (F :++: G)#Out] =
    new Inject[F, (F :++: G)#Out] {
      def inj[A](fa: F[A]): Coproduct[F, G, A] = Coproduct(Left(fa))
    }

  implicit def injectRight[F[_], G[_], H[_]](implicit I: Inject[F, H]): Inject[F, (G :++: H)#Out] =
    new Inject[F, (G :++: H)#Out] {
      def inj[A](fa: F[A]): Coproduct[G, H , A] = Coproduct(Right(I.inj(fa)))
    }
}

object Test1 {
  import Coproduct.{:++:, :+:}

  class Foo[A]
  class Bar[A]
  class Baz[A]

  implicitly[Inject[Baz, (Foo :+: Bar :++: Baz)#Out]]

  implicitly[Inject[Baz, ({ type Out[A] = Coproduct[Foo, ({ type Out1[a] = Coproduct[Bar, Baz, a] })#Out1, A] })#Out]]
}
