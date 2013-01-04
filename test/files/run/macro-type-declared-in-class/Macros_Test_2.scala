class Macros {
  type Foo(x: Int) = macro Impls.foo
}

object Test extends App {
  val prefix = new Macros()
  class D extends prefix.Foo(2)
  val x: prefix.Foo(2) = new D
}