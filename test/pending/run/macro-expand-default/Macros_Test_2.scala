object Test extends App {
  def foo(x: Int = 2, y: Int = -40) = macro Impls.foo
  foo(y = -40, x = 2)
  foo(x = 2, y = -40)
  foo(x = 100)
  foo(y = 100)
  foo()
}