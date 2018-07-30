trait Foo[T]
object Foo {
  implicit val fooInt: Foo[Int] = ???
  implicit def fooPair[H, T](implicit h: Foo[H], t: Foo[T]): Foo[(H, T)] = ???
}

trait Bar
object Bar {
  implicit def fooBar(implicit repr: => Foo[(Int, (Int, Int))]): Foo[Bar] = ???
}

trait Baz
object Baz {
  implicit def fooBaz(implicit i: Foo[Int], rec: => Foo[Baz]): Foo[Baz] = ???
}

object Test {
  implicitly[Foo[Int]]
  implicitly[Foo[(Int, Int)]]
  implicitly[Foo[(Int, (Int, Int))]]
  implicitly[Foo[(Int, (Int, (Int, Int)))]]
  implicitly[Foo[Bar]]
  implicitly[Foo[(Int, Bar)]]
  implicitly[Foo[Baz]]
}
