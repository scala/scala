sealed trait Foo[+A]

final case class Bar(x: Int) extends Foo[Nothing]
final case class Baz[A](x: A) extends Foo[A]

sealed trait TC[A]

object TC {
  implicit val intTC: TC[Int] = new TC[Int] {}
  implicit def fooTC[A](implicit ev: TC[A]): TC[Foo[A]] = new TC[Foo[A]] {}
}

object Test {
  def handler[A](foo: Foo[A])(implicit ev: TC[A]): TC[A] = ev

  val h0 = handler(Baz(23))
  h0: TC[Int]

  val h1 = handler(Bar(23))
  h1: TC[Foo[Int]]

  val h2: TC[Int] = handler(Bar(23))

  val h3 = handler(Bar(23))(TC.fooTC(TC.intTC))
  h3: TC[Foo[Int]]

  val h4 = handler[Int](Bar(23))
  h4: TC[Int]

  val h5 = handler[Foo[Int]](Bar(23))
  h5: TC[Foo[Int]]

  val h6 = handler[Foo[Foo[Int]]](Bar(23))
  h6: TC[Foo[Foo[Int]]]
}
