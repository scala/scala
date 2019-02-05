trait Foo[T]

object Foo {
  implicit val int: Foo[Int] = ???
  implicit val bool: Foo[Boolean] = ???
  implicit def pair[T, U](implicit ftu0: => Foo[(T, U)], ftu1: => Foo[(T, U)]): Foo[(T, U)] = ???
}

object Test {
  implicitly[Foo[(Int, Boolean)]]
}
