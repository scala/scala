import scala.collection.mutable.ListBuffer

class A
class B extends A
class C extends B

trait FF1[T, +R]
trait FF2[-T, R]

class Inv[T]
class InvA extends Inv[A]
class InvB extends Inv[B]
class InvC extends Inv[C]

class Multi[+Cov, Inv, -Con]
class MultiCov[+T <: A] extends Multi[T, B, C]
class MultiInv[T] extends Multi[A, T, C]
class MultiCon[-T >: C] extends Multi[A, B, T]

object Test {
  def f1 = Set[Inv[A]]() + new Inv[A]
  def f2 = Set[Inv[A]]() + new Inv[B]
  def f3 = Set[Inv[A]]() + new Inv[C]
  def f4 = Set[Inv[B]]() + new Inv[A]
  def f5 = Set[Inv[B]]() + new Inv[B]
  def f6 = Set[Inv[B]]() + new Inv[C]
  def f7 = Set[Inv[C]]() + new Inv[A]
  def f8 = Set[Inv[C]]() + new Inv[B]
  def f9 = Set[Inv[C]]() + new Inv[C]

  def g1 = Set[Multi[A, B, C]]() + new MultiCov[A]
  def g2 = Set[Multi[A, B, C]]() + new MultiCov[B]
  def g3 = Set[Multi[A, B, C]]() + new MultiCov[C]
  def g4 = Set[Multi[A, B, C]]() + new MultiInv[A]
  def g5 = Set[Multi[A, B, C]]() + new MultiInv[B]
  def g6 = Set[Multi[A, B, C]]() + new MultiInv[C]
  def g7 = Set[Multi[A, B, C]]() + new MultiCon[A]
  def g8 = Set[Multi[A, B, C]]() + new MultiCon[B]
  def g9 = Set[Multi[A, B, C]]() + new MultiCon[C]
}

object Functions {
  object Set1 {
    def f[T, R](x: FF1[T, R]) = ()
    def h[T, R] : FF1[T, R] = sys.error("")

    def ff1 = f[B, B](h[A, A]) // fail
    def ff2 = f[B, B](h[B, A]) // fail
    def ff3 = f[B, B](h[C, A]) // fail
    def ff4 = f[B, B](h[A, B]) // suggest
    def ff5 = f[B, B](h[B, B]) // ok
    def ff6 = f[B, B](h[C, B]) // suggest
    def ff7 = f[B, B](h[A, C]) // suggest
    def ff8 = f[B, B](h[B, C]) // ok
    def ff9 = f[B, B](h[C, C]) // suggest
  }
  object Set2 {
    def f[T, R](x: FF2[T, R]) = ()
    def h[T, R] : FF2[T, R] = sys.error("")

    def ff1 = f[B, B](h[A, A]) // suggest
    def ff2 = f[B, B](h[B, A]) // suggest
    def ff3 = f[B, B](h[C, A]) // fail
    def ff4 = f[B, B](h[A, B]) // ok
    def ff5 = f[B, B](h[B, B]) // ok
    def ff6 = f[B, B](h[C, B]) // fail
    def ff7 = f[B, B](h[A, C]) // suggest
    def ff8 = f[B, B](h[B, C]) // suggest
    def ff9 = f[B, B](h[C, C]) // fail
  }
}

// TODO
// object TypeAlias {
//   type LL[T] = List[T]
//   val LL = List
//
//   def f1 = Set[LL[B]]() + LL[A](new A)
//   def f2 = Set[LL[B]]() + LL[C](new C)
// }

object Javas {
  def f[T](x: java.util.List[T]) = ()
  def g[T](x: java.util.Comparator[T]) = ()

  def g1 = f[AnyRef](new java.util.ArrayList[String] { })
  def g2 = g[String](Ordering.fromLessThan[AnyRef](_.toString < _.toString))
}

object Misc {
  // original motivation
  class Data[A <: AnyVal]
  class MyData extends Data[Int] { }
  def f1 = Set[Data[AnyVal]]() + new MyData

  // from stackoverflow
  def foo(s: Set[CharSequence]): Unit = ()
  def f4 = {
    val s: Set[String] = Set("Hello", "World");
    foo(s)
  }

  class Trippy[+T1, T2, +T3]
  def g1 = Set[Trippy[AnyRef, AnyRef, AnyRef]]() + new Trippy[String, String, String]
  def g2 = Set[Map[String, String]]() + Map[AnyRef, String]()
}