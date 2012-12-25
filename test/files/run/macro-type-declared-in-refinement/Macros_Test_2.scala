class Base

object Test extends App {
  val macros = new Base { type Foo(x: Int) = macro Impls.foo }
  class D extends macros.Foo(2)
  val x: macros.Foo(2) = new D
}