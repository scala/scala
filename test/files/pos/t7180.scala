trait Higher[F[_]]

trait Box[A]
object Box {
  implicit def HigherBox = new Higher[Box] {}
}

object Foo {
  val box = implicitly[Higher[Box]] // compiles fine !!!

  type Bar[A] = Box[A]
  val bar = implicitly[Higher[Bar]] // <-- this doesn't compile in 2.10.1-RC1, but does in 2.10.0 !!!
}
