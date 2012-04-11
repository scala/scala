object Test extends App {
  def foo(x: Int) = macro Impls.foo
  println(foo(42))
}