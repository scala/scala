import scala.language.higherKinds

case class Const[A, B](value: A)
sealed trait Foo[F[_], A]
final case class Bar[F[_]]() extends Foo[F, Unit]

object Test {
  def f[F[_], A](foo: Foo[F, A]): Unit = foo match {
    case Bar() => Const[Unit, F[Unit]](()).value
  }
}
