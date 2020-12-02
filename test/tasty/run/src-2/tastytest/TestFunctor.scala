package tastytest

object TestFunctor extends Suite("TestFunctor") {

  implicit object ListFunctor extends Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit object ListFunctorI extends FunctorI[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit object ListFunctorL extends FunctorL[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  def toStringOnFunctorL[F[X] <: List[X]: FunctorL, A](fa: F[A]): F[String] = implicitly[FunctorL[F]].map(fa)(_.toString())
  def toStringOnFunctor[F[_]: Functor, A](fa: F[A]): F[String]   = implicitly[Functor[F]].map(fa)(_.toString())
  def hashOnFunctorI[F[_]: FunctorI, A <: Int](fa: F[A]): F[Int] = implicitly[FunctorI[F]].map(fa)(_.##)

  test(assert(toStringOnFunctor(List(true,false)) === List("true","false")))
  test(assert(hashOnFunctorI(List(1,2)) === List(1,2)))
  test(assert(toStringOnFunctorL(List(true,false)) === List("true","false")))

}
