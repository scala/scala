class foo(val bar: Int) extends annotation.StaticAnnotation

object Api {
  @foo({ type Foo(x: Int) = macro Impls.foo; class D extends Foo(2); val x: Foo(2) = new D; x.x })
  def foo = println("it works")
}