object Test {
  class Foo
  trait Bar {
    trait Baz
    object Baz {
      implicit val qb: Quux[Baz] = ???
    }
    def baz(implicit quux: Quux[Baz]): Unit = ???
  }
  implicit def mkBar(foo0: Foo): Bar = ???

  trait Quux[T]
  object Quux {
    implicit def mkQuux[T]: Quux[T] = ???
  }

  (new Foo).baz
}

