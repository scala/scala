object HKGADT {
  sealed trait Foo[F[_]]
  final case class Bar() extends Foo[List]

  def frob[F[_]](foo: Foo[F]): F[Int] =
    foo match {
      case Bar() => Set(1)
    }

  sealed trait Foo1[F]
  final case class Bar1() extends Foo1[Int]
  def frob1[A](foo: Foo1[A]): A = foo match {
    case Bar1() => true
  }
}

object HKGADT2 {
  sealed trait Foo[F[_]]
  final case class Bar() extends Foo[List]
  final case class Baz() extends Foo[Set]

  def frob[F[_]](foo: Foo[F]): F[Int] =
    foo match {
      case Bar() => Set(1)
      case Baz() => List(1)
    }

  sealed trait Foo1[F]
  final case class Bar1() extends Foo1[Int]
  final case class Baz1() extends Foo1[Boolean]
  def frob1[A](foo: Foo1[A]): A = foo match {
    case Bar1() => true
    case Baz1() => 1
  }
}
