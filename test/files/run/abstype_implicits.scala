import scala.language.higherKinds

trait Functor[F[_]]

package data {
  trait MaybeModule {
    type Maybe[_]
    def some[A](a: A): Maybe[A]
    def functorInstance: Functor[Maybe]
  }

  object MaybeModule {
    implicit def functorInstance: Functor[Maybe] = Maybe.functorInstance
  }

  private[data] object MaybeImpl extends MaybeModule {
    type Maybe[A] = Option[A]
    def some[A](a: A): Maybe[A] = Some(a)
    def functorInstance: Functor[Maybe] = new Functor[Option] {}
  }
}

package object data {
  val Maybe: MaybeModule = MaybeImpl
  type Maybe[A] = Maybe.Maybe[A]
}

class Foo
object Foo {
  import data.Maybe

  implicit val maybeFoo: Maybe[Foo] = Maybe.some(new Foo)
}

object Test extends App {
  import data.Maybe

  implicitly[Functor[Maybe]]
  implicitly[Maybe[Foo]]
}
