object Test extends App {
  def foo[T](x: T) = macro Impls.foo[T]
  println(foo(42))
}