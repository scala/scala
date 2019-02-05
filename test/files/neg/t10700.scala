object Test {
  class Foo[F[_]]
  def foo(b: Boolean) = if (b) new Foo[List] else new Foo[Option]
}
