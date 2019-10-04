package tastytest

object TestFunctor {

  implicit object ListFunctor extends Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit object ListFunctorI extends FunctorI[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit object ListFunctorL extends FunctorL[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  def toStringOnFunctorL[F[_] <: List[_]: FunctorL, A](fa: F[A]): F[String] = implicitly[FunctorL[F]].map(fa)(_.toString())
  def toStringOnFunctor[F[_]: Functor, A](fa: F[A]): F[String]   = implicitly[Functor[F]].map(fa)(_.toString())
  def hashOnFunctorI[F[_]: FunctorI, A <: Int](fa: F[A]): F[Int] = implicitly[FunctorI[F]].map(fa)(_.##)

  def test1 = assert(toStringOnFunctor(List(true,false)) === List("true","false"))
  def test2 = assert(hashOnFunctorI(List(1,2)) === List(1,2))
  def test3 = assert(toStringOnFunctorL(List(true,false)) === List("true","false"))

  def main(args: Array[String]): Unit = {
    test1
    test2
    test3
    println("Suite passed!")
  }
}
