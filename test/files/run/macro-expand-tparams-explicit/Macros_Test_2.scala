object Test extends App {
  def foo[U]: Unit = macro Impls.foo[U]
  foo[Int]
}