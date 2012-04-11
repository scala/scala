object Test extends App {
  def foo[U](x: U) = macro Impls.foo[U]
  foo(42)
  foo("42")
}