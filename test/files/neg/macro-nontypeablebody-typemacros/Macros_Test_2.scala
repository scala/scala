object Test extends App {
  val macros = new { type Foo(x: Int) = macro Impls.foo2 }
  class D1 extends macros.Foo(2)
  val x1: macros.Foo(2) = new D1

  type Foo(x: Int) = macro Impls.foo2
  class D2 extends Foo(2)
  val x2: Foo(2) = new D2
}