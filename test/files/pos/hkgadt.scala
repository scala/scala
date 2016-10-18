package test

object HKGADT {
  sealed trait Foo[F[_]]
  final case class Bar() extends Foo[List]

  def frob[F[_]](foo: Foo[F]): F[Int] =
    foo match {
      case Bar() =>
         List(1)
    }

  sealed trait Foo1[F]
  final case class Bar1() extends Foo1[Int]
  def frob1[A](foo: Foo1[A]) = foo match {
    case Bar1() => 1
  }
}
