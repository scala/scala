object Test extends App {
  implicit def foo[T]: List[T] = macro Impls.foo[T]
  def bar[T](implicit foo: List[T]) {}
  implicitly[List[Int]]
  bar[String]
}