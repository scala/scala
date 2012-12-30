class Macros {
  object Macros {
    type Foo(x: Int) = macro Impls.foo
  }
}

object Test extends App {
  val outer = new Macros()
  class D extends outer.Macros.Foo(2)
  val x: outer.Macros.Foo(2) = new D
}