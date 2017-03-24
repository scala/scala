sealed trait Foo[+A]

final case class Bar(x: Int) extends Foo[Nothing]
final case class Baz[A](x: A) extends Foo[A]

sealed trait TC[A]

object TC extends TC0 {
  implicit def ambig0[A]: TC[A] = new TC[A] {}
  implicit def ambig1[A <: AnyRef]: TC[A] = new TC[A] {}
}

trait TC0 {
  implicit def fooTC[A](implicit ev: TC[A]): TC[Foo[A]] = new TC[Foo[A]] {}
  implicit val intTC: TC[Int] = new TC[Int] {}
}

object Test {
  def handler[A](foo: Foo[A])(implicit ev: TC[A]): TC[A] = ev

  val h0 = handler(Baz(23))
  h0: TC[Int]

  val h1 = handler(Bar(23))
  h1: TC[Foo[Int]]

  val h2: TC[Int] = handler(Bar(23))

  val h4 = handler[Int](Bar(23))
  h4: TC[Int]

  val h6 = handler[Foo[Foo[Int]]](Bar(23))
  h6: TC[Foo[Foo[Int]]]
}
