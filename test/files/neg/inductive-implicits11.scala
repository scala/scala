trait Foo[T, U]
object Foo {
  implicit def tick[T, U](implicit f: Foo[Option[T], U]): Foo[T, Option[U]] = ???
  implicit def tock[T, U](implicit f: Foo[T, Option[U]]): Foo[Option[T], U] = ???
  implicit val stop: Foo[Int, Int] = ???
}

object Test {
  implicitly[Foo[Option[Int], Int]]
}
