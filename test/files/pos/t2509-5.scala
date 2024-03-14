// See https://github.com/scala/scala3/issues/2974
//> using options -Xsource:3 -Xsource-features:implicit-resolution

trait Foo[-T]

trait Bar[-T] extends Foo[T]

object Test {
  implicit val fa: Foo[Any] = ???
  implicit val ba: Bar[Int] = ???

  def test: Unit = {
    implicitly[Foo[Int]]
  }
}
