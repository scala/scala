class Foo {
  def foo(a: Int) = a
}

object Test {
  val a = new Foo
  a.foo() /*#*/
}