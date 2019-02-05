trait Foo[T]

object Foo {
  implicit def pair[T, U](implicit fooT: => Foo[(T, U)], fooU: => Foo[(U, T)]): Foo[(T, U)] = new Foo[(T, U)] {}
  implicit def int: Foo[Int] = new Foo[Int] {}
  implicit def string: Foo[String] = new Foo[String] {}
}

object Test {
  implicitly[Foo[(Int, String)]]
}
