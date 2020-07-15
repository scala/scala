package tastytest

object TestBiFunctor extends Suite("TestBiFunctor") {

  implicit object Tuple2BiFunctor extends BiFunctor[Tuple2] {
    def bimap[A,B,C,D](fab: (A,B))(f: A => C, g: B => D): (C,D) = (f(fab._1), g(fab._2))
  }

  implicit object Tuple2BiFunctorT2 extends BiFunctorT2[Tuple2] {
    def bimap[A,B,C,D](fab: (A,B))(f: A => C, g: B => D): (C,D) = (f(fab._1), g(fab._2))
  }

  def toStringOnBiFunctor[F[_,_]: BiFunctor, A,B](fab: F[A,B]): F[String, String] =
    implicitly[BiFunctor[F]].bimap(fab)(_.toString, _.toString)

  def toStringOnBiFunctorT2[F[_,_] <: Tuple2[_,_]: BiFunctorT2, A,B](fab: F[A,B]): F[String, String] =
    implicitly[BiFunctorT2[F]].bimap(fab)(_.toString, _.toString)

  test(assert(toStringOnBiFunctor((true,1)) === ("true","1")))
  test(assert(toStringOnBiFunctorT2((true,1)) === ("true","1")))

}
