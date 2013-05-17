package library.x {
  class X {
    class Foo
    implicit val foo = new Foo
  }
}
package library { package object x extends X }
package app {
  import library.x._
  object App { implicitly[Foo] }
}
