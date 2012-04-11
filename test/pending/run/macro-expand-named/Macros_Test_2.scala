object Test extends App {
  def foo(x: Int, y: Int) = macro Impls.foo
  foo(y = -40, x = 2)
  foo(x = 2, y = -40)
}