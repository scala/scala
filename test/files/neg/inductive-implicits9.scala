trait Foo[A]
object Foo {
  implicit def materialize[A]: Foo[A] = new Foo[A] {}
}

object Test {
  def test {
    implicit val a = new Foo[String] { }
    implicit val b = new Foo[String] { }
    implicitly[Foo[String]]
  }
}
