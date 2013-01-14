object Test extends App {
  def foo1(x: Int)(y: Int) = macro Impls.foo
  def foo2(x: _)(y: Int) = macro Impls.foo
  def foo3(x: Int)(y: _) = macro Impls.foo
  def foo4(x: _)(y: _) = macro Impls.foo

  foo1(40)(2)
  foo2(40)(2)
  foo3(40)(2)
  foo4(40)(2)
}