package tastytest

object TestFunctor {

  implicit object ListMonad extends Monad[List] {

    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    def pure[A](x: A): List[A] = x :: Nil

    // error: Missing implementation for:
    //   def map[A, B](fa: Monad.this.F[A])(f: A => B): Monad.this.F[B] // inherited from trait Functor
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

  }

  def pureToString[F[_], A](fa: F[A])(implicit F: Monad[F]): F[String] = F.flatMap(fa)(a => F.pure(a.toString))

  def test1 = assert(pureToString(List(1,2,3)) === List("1","2","3"))

  def main(args: Array[String]): Unit = {
    test1
    println("Suite passed!")
  }
}