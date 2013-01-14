object Test extends App {
  def foo[U] = macro Impls.foo[U]
  foo[Int]
}