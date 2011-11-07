object Basis {
  final case class X[T](t: T)
  val x  = Seq(X(32))
  val y  = Seq(X(true))
  val x1 = Seq(X("asdf"))
  val x2 = Seq(X('d'))
}
import Basis._

object DoesWork {
  // Doesn'tWork
  // def f1 = x ++ y ++ x1 ++ x2

  def f2 = List(x, y, x1, x2).flatten
}

// Testing the not giving of explicit Booper[M] arguments.
object ShouldWorkHK {
  class Booper[M[_]](xs: Seq[M[_]]) extends collection.generic.SeqForwarder[M[_]] {
    def underlying = xs
    def BOOP(ys: Seq[M[_]]) = new Booper(xs ++ ys)
  }
  implicit def mkBoop[M[_]](xs: Seq[M[_]]) = new Booper(xs)

  def f1 = x BOOP y BOOP x1 BOOP x2
}

object DoesWorkHK {
  class Booper[M[_]](xs: Seq[M[_]]) extends collection.generic.SeqForwarder[M[_]] {
    def underlying = xs
    def BOOP(ys: Seq[M[_]]) = new Booper[M](xs ++ ys)
  }
  implicit def mkBoop[M[_]](xs: Seq[M[_]]) = new Booper[M](xs)
  
  def f1 = x BOOP y BOOP x1 BOOP x2
}

