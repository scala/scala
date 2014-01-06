
package library.x {
  class X {
    class Foo
    implicit val foo: Foo = new Foo
  }
}
package library {
  package object y extends library.x.X
}

object ko {
  import library.y.{Foo, foo}
  implicitly[Foo]
}

object ko2 {
  import library.y._
  implicitly[Foo]
}
