object Macros {
  type Foo(x: Int) = macro Impls.foo
}

object Test extends App {
  class D extends Macros.Foo(2)
  val x: Macros.Foo(2) = new D
}