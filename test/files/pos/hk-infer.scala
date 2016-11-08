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

trait TraversableForwarder[+A] extends Traversable[A] {
  /** The traversable object to which calls are forwarded. */
  protected def underlying: Traversable[A]
}
trait IterableForwarder[+A] extends Iterable[A] with TraversableForwarder[A] {
  protected def underlying: Iterable[A]
}
trait SeqForwarder[+A] extends Seq[A] with IterableForwarder[A] {
  protected override def underlying: Seq[A]
}

// Testing the not giving of explicit Booper[M] arguments.
object ShouldWorkHK {
  class Booper[M[_]](xs: Seq[M[_]]) extends SeqForwarder[M[_]] {
    def underlying = xs
    def iterator: Iterator[M[_]] = ???
    def apply(idx: Int): M[_] = ???
    def length: Int = ???
    def BOOP(ys: Seq[M[_]]) = new Booper(xs ++ ys)
  }
  implicit def mkBoop[M[_]](xs: Seq[M[_]]) = new Booper(xs)

  def f1 = x BOOP y BOOP x1 BOOP x2
}

object DoesWorkHK {
  class Booper[M[_]](xs: Seq[M[_]]) extends SeqForwarder[M[_]] {
    def underlying = xs
    def iterator: Iterator[M[_]] = ???
    def apply(idx: Int): M[_] = ???
    def length: Int = ???
    def BOOP(ys: Seq[M[_]]) = new Booper[M](xs ++ ys)
  }
  implicit def mkBoop[M[_]](xs: Seq[M[_]]) = new Booper[M](xs)

  def f1 = x BOOP y BOOP x1 BOOP x2
}
