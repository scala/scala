object Test extends App {
  def foo[U: Numeric](x: U) = macro Impls.foo[U]
  foo(42)
}