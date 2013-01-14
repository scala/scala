object Test extends App {
  def foo(bar: Int = { type Foo(x: Int) = macro Impls.foo; class D extends Foo(2); val x: Foo(2) = new D; x.x }) = println(bar)

  foo()
  foo(100)
  foo()
}