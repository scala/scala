object Macros {
  class Macros {
    type Foo(x: Int) = macro Impls.foo
  }
}

object Test extends App {
  val outer = Macros
  val prefix = new outer.Macros()
  class D extends prefix.Foo(2)
  val x: prefix.Foo(2) = new D
}