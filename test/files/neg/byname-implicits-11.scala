trait Foo[T]

object Foo {
  implicit def foo[T](implicit fooFoo: => Foo[Foo[T]]): Foo[T] = ???
}

object Test {
  implicitly[Foo[Int]]
}
