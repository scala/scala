package moduleclass

final class Foo[A](implicit f: Factory[A])

object Factory {
  implicit val factory: Factory[Bar] = ???
}

trait Factory[A]

object A {
  val fails = new Foo[Bar]()
}
