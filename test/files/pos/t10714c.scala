object Test {
  class Foo
  trait Bar {
    val foo: Foo
    def baz(implicit quux: Quux[foo.type]): Unit = ???
  }
  implicit def mkBar(foo0: Foo): Bar { val foo: foo0.type } = ???

  trait Quux[T]
  object Quux {
    implicit def mkQuux[T]: Quux[T] = ???
  }

  (new Foo).baz
}

