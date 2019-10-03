package tastytest

/**
 * blocked by https://github.com/lampepfl/dotty/issues/7328
 */
object TestReader {

  implicit def mkReaderMonad[Ctx]: Reader[Ctx] = new Reader[Ctx]() {}

  def pureToString[F[_], A](fa: F[A])(implicit F: Monad[F]): F[String] = F.flatMap(fa)(a => F.pure(a.toString))

  def test1 = {
    val f = pureToString((s: Unit) => 101)
    assert(f(()) === "101")
  }

  def main(args: Array[String]): Unit = {
    test1
    println("Suite passed!")
  }
}
