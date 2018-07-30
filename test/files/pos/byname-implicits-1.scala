trait Foo
object Foo {
  implicit def foo(implicit rec: => Foo): Foo = ???
}

object Test {
  implicitly[Foo]
}
