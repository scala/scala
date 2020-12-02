package tastytest

object TestMonad extends Suite("TestMonad") {

  implicit object ListMonad extends Monad[List] {
    def pure[A](x: A): List[A] = x :: Nil
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  def pureToString[F[_], A](fa: F[A])(implicit F: Monad[F]): F[String] =
    F.flatMap(fa)(a => F.pure(a.toString))

  test(assert(pureToString(List(1,2,3)) === List("1","2","3")))

}
