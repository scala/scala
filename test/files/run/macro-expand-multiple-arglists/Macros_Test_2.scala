object Test extends App {
  def foo(x: Int)(y: Int) = macro Impls.foo
  foo(40)(2)
}