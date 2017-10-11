sealed trait Foo[F[_]]
case class Bar[F[_]]() extends Foo[F]

trait TC[A, B] {
  type F[X] = B
}

object TC {
  implicit val intInstance: TC[Int, String] =
    new TC[Int, String] {}

  implicit class Ops[A, B](a: A)(implicit val tc: TC[A, B]) {
    def getFoo: Foo[tc.F] = ???
  }

  1.getFoo match {
    case Bar() =>
  }
}
