trait Foo[T]
object Foo {
  implicit def unit: Foo[Unit] = ???
  implicit def int: Foo[Int] = ???
  implicit def pair[T, U](implicit ft: Foo[T], fu: Foo[U]): Foo[(T, U)] = ???
}

class Bar
object Bar {
  implicit def bar(implicit f: => Foo[(Int, (Int, Unit))]): Foo[Bar] = ???
}

object Test {
  implicitly[Foo[(Bar, Unit)]]
}
