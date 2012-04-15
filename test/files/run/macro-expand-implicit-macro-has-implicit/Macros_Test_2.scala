object Test extends App {
  implicit val x = 42
  def foo(implicit x: Int) = macro Impls.foo
  foo
}