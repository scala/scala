// See https://github.com/lampepfl/dotty/issues/2974
// scalac: -Xsource:3.0

trait Foo[-T]

trait Bar[-T] extends Foo[T]

object Test {
  implicit val fa: Foo[Any] = ???
  implicit val ba: Bar[Int] = ???

  def test: Unit = {
    implicitly[Foo[Int]]
  }
}
