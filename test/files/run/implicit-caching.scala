trait Foo[T]

trait FooSub[T] extends Foo[T] {
  type Super = Foo[T]
}

object FooSub {
  implicit def fooSub[T](implicit ft: Bar[T]): FooSub[T] =
    new FooSub[T] {}
}

trait Bar[T]

class Quux

object Quux {
  implicit val barQuux: Bar[Quux] = new Bar[Quux] {}

  val fooSubQuux = implicitly[FooSub[Quux]]
  implicit val fooQuux: fooSubQuux.Super = fooSubQuux
}

object Test extends App {
  implicitly[Foo[Quux]]
}
