sealed trait Foo[+A]
final case class Bar(x: Int) extends Foo[Nothing]
final case class Baz[A](x: A) extends Foo[A]

sealed trait TC[A]

trait TCImplicits extends TCLowerImplicits {
  implicit def fooTC[A](implicit tca: TC[A]): TC[Foo[A]] = new TC[Foo[A]] {}
}

trait TCLowerImplicits {
  implicit val intTC: TC[Int] = new TC[Int] {}
}

object test extends TCImplicits {
  def handler[A](foo: Foo[A])(implicit ev: TC[A]): TC[A] = ev

  val h0a = handler(Bar(23))(fooTC[Int](intTC))
  h0a: TC[Foo[Int]]

  val h0 = handler(Bar(23))
  h0: TC[Foo[Int]]

  val h1 = handler(Baz(23))
  h1: TC[Int]
}
