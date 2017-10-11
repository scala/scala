sealed trait Foo[A, F[_ <: A]]
case class Bar[A, F[_ <: A]]() extends Foo[A, F]

class F[S <: String]

object Test {
  def f(foo: Foo[String, F]): Unit = foo match {
    case Bar() => ()
  }
}
