sealed trait Foo[+A]

final case class Bar(x: Int) extends Foo[Nothing]

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

  //val h3 = handler(Bar(23))(TC.fooTC(TC.intTC))
  //h3: TC[Foo[Int]]

  val h5 = handler[Foo[Int]](Bar(23))
  //h5: TC[Foo[Int]]
}
