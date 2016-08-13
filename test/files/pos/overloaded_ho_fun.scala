import scala.math.Ordering
import scala.reflect.ClassTag

trait Sam { def apply(x: Int): String }
trait SamP[U] { def apply(x: Int): U }

class OverloadedFun[T](x: T) {
  def foo(f: T => String): String = f(x)
  def foo(f: Any => T): T = f("a")

  def poly[U](f: Int => String): String = f(1)
  def poly[U](f: Int => U): U = f(1)

  def polySam[U](f: Sam): String = f(1)
  def polySam[U](f: SamP[U]): U = f(1)

  // check that we properly instantiate java.util.function.Function's type param to String
  def polyJavaSam(f: String => String) = 1
  def polyJavaSam(f: java.util.function.Function[String, String]) = 2
}

class StringLike(xs: String) {
  def map[A](f: Char => A): Array[A] = ???
  def map(f: Char => Char): String = ???
}

object Test {
  val of = new OverloadedFun[Int](1)

  of.foo(_.toString)

  of.poly(x => x / 2 )
  of.polySam(x => x / 2 )
  of.polyJavaSam(x => x)

  val sl = new StringLike("a")
  sl.map(_ == 'a')  // : Array[Boolean]
  sl.map(x => 'a')  // : String
}

object sorting {
  def stableSort[K: ClassTag](a: Seq[K], f: (K, K) => Boolean): Array[K] = ???
  def stableSort[L: ClassTag](a: Array[L], f: (L, L) => Boolean): Unit = ???

  stableSort(??? : Seq[Boolean], (x: Boolean, y: Boolean) => x && !y)
}

// trait Bijection[A, B] extends (A => B) {
//   def andThen[C](g: Bijection[B, C]): Bijection[A, C] = ???
//   def compose[T](g: Bijection[T, A]) = g andThen this
// }
