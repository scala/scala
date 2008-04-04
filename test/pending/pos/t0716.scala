trait Functor[F[_]] {
  def fmap[A,B](fun: A=>B, arg:F[A]): F[B]
}
object Functor{
  implicit val ListFunctor = new Functor[List] {
    def fmap[A, B](f: A => B, arg: List[A]):List[B] = arg map f
  }

  final class OOFunctor[F[_],A](arg:F[A])(implicit ftr: Functor[F]) {
    def fmap[B](fun: A=>B):F[B] = ftr.fmap(fun,arg)
  }

  //breaks if uncommented
  implicit def lifttoOO[F[_],A](arg:F[A])(implicit ftr: Functor[F]) = new OOFunctor[F,A](arg)(ftr)

  //works if uncommented
  //implicit def liftListtoOO[A](arg:List[A]):OOFunctor[List,A] = new OOFunctor[List,A](arg)
}

object GeneralLiftingDemo extends Application {
  import Functor._
  val l = List(1,2,3)
  println("OO : " + l.fmap( 1+) )
}
