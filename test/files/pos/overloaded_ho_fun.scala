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

object SI10194 {
  trait X[A] {
    def map[B](f: A => B): Unit
  }

  trait Y[A] extends X[A] {
    def map[B](f: A => B)(implicit ordering: Ordering[B]): Unit
  }

  trait Z[A] extends Y[A]

  (null: Y[Int]).map(x => x.toString) // compiled
  (null: Z[Int]).map(x => x.toString) // didn't compile
}

// Perform eta-expansion of methods passed as functions to overloaded functions
trait A { def map[T](f: Int => T): Unit = () }
object B extends A {
  def noover(f: SAM[Int, Any]): Unit = ()
  def map[T: scala.reflect.ClassTag](f: Int => T): Unit = ()
  def f(x: Int) = x
  noover(f)
  noover(identity)
  map(f) // same param type, monomorphic method
  map(identity) // same param type, polymorphic method

  // must not lub to an incompatible function type
  object t { def f(x: Int => Int): Int = 0; def f(y: String => Int): String = "1" }
  def fun(x: Int) = x
  def fun2[T] = (x: T) => 42
  t.f(fun) // different param type, monomorphic method
  //t.f(fun2) // different param type, polymorphic method - not possible
}

// The same for SAM types
trait SAM[-T, +R] { def apply(x: T): R }
trait A2 { def map[T](f: SAM[Int, T]): Unit = () }
object B2 extends A2 {
  def noover(f: SAM[Int, Any]): Unit = ()
  def map[T: scala.reflect.ClassTag](f: SAM[Int, T]): Unit = ()
  def f(x: Int) = x
  noover(f)
  noover(identity)
  map(f) // same param type, monomorphic method
  //map(identity) // same param type, polymorphic method - not possible for SAMs

  // must not lub to an incompatible function type
  object t { def f(x: SAM[Int, Int]): Int = 0; def f(y: SAM[String, Int]): String = "1" }
  def fun(x: Int) = x
  def fun2[T] = (x: T) => 42
  t.f(fun) // different param type, monomorphic method
  //t.f(fun2) // different param type, polymorphic method - not possible
}
