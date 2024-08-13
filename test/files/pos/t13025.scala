package p1 {
  case class Foo[X] private (a: Int)
  object Foo {
    def apply[X](a: String): Foo[Any] = ???
  }
}
package p2 {
  class C {
    def x = p1.Foo[Any](0)
  }
}
