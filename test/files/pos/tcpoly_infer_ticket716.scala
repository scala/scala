
trait Functor[F[_]] {
  def fmap[A,B](fun: A=>B, arg:F[A]): F[B]
}
object Functor{
  implicit val ListFunctor: Functor[List] = new Functor[List] {
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

object GeneralLiftingDemo extends App {
  import Functor._
  val l = List(1,2,3)
  val res = l fmap( 1+) // TODO: should not need explicit call to lifttoOO
  println("OO : " + res )
}
