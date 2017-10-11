sealed trait Foo[F[_]]
case class Bar[F[_]]() extends Foo[F]

object Test {

  val foo: Foo[({ type Out[X] = String })#Out] = ???

  foo match {
    case Bar() =>
  }
}
