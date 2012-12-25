object Test extends App {
  type Foo(x: Int) = macro Impls.foo
  class D extends Foo(2)
  val x: Foo(2) = new D
}