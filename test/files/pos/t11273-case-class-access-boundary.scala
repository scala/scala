package p1 {
  private[p1] case class Foo(a: Int)
  object Foo {
    def m = 1
  }
}

package p2 {
  class Test {
    p1.Foo.m
    p1.Foo(42)
  }
}
