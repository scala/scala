//> using options -Xfatal-warnings -Xlint:infer-any
//

import scala.language.implicitConversions

trait Foo[-A <: AnyRef, +B <: AnyRef] {
  def run[U](x: A)(action: B => U): Boolean = ???

  def foo = { run(_: A)(_: B => String) }
}

trait Xs[+A] {
  { List(1, 2, 3) contains "a" }  // only this warns
  { List(1, 2, 3) contains 1 }
  { identity(List(1, 2, 3) contains 1) }
  { List("a") foreach println }
}

trait Ys[+A] {
  { 1 to 5 contains 5L }          // should warn: scala.Predef.intWrapper(1).to(5).contains[AnyVal](5L)
  { 1L to 5L contains 5 }         // warn
  { 1L to 5L contains 5d }        // warn
  { 1L to 5L contains 5L }
}

trait Zs {
  def f[A](a: A*) = 42
  def g[A >: Any](a: A*) = 42     // don't warn

  def za = f(1, "one")
  def zu = g(1, "one")
}

class C1
class C2

trait Cs {
  val cs = List(new C1)
  cs.contains[AnyRef](new C2)     // doesn't warn
  cs.contains(new C2)             // warns
}

object t11798 {

  trait ZIO[-R, +E, +A]
  type Task[A] = ZIO[Any, Throwable, A]  // explicit Any

  trait ZStream[-R, +E, +A] {
    def mapM[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): ZStream[R1, E1, B] =
      ???
  }

  val stream: ZStream[Any, Throwable, Int] = ???
  def f(n: Int): Task[Int] = ???
  stream.mapM(f)                  // should not warn
  stream.mapM(n => (f(n): ZIO[Any, Throwable, Int]))
  stream.mapM(f: Int => ZIO[Any, Throwable, Int])
}

/**
 * 1 to 5 contains 5L fails to warn, because silent mode due to overload
 *
scala> :type 1 to 5
scala.collection.immutable.Range.Inclusive

scala> :type 1L to 5L
scala.collection.immutable.NumericRange.Inclusive[Long]

warning: !!! HK subtype check on scala.this.Int and [B >: scala.this.Int]B, but both don't normalize to polytypes:
  tp1=scala.this.Int       ClassNoArgsTypeRef
  tp2=[B >: scala.this.Int]B PolyType
[log typer] infer method alt value contains with alternatives List([B >: scala.this.Int](<param> elem: B)scala.this.Boolean, (<param> x: scala.this.Int)scala.this.Boolean) argtpes=List(5L) pt=?
[log typer] infer method inst scala.Predef.intWrapper(1).to(5).contains[B], tparams = List(type B), args = List(scala.this.Long(5L)), pt = ?, lobounds = List(scala.this.Int), parambounds = List( >: scala.this.Int)
[log typer] checkKindBounds0(List(type B), List(scala.this.AnyVal), <noprefix>, <none>, true)
 */

// from scala/scala#11053, which was reverted
class t12044 extends App {
  class Bar
  def f[F[_], A](v: F[A]) = v
  implicit def barToList(b: Bar): List[Int] = List(42)
  def t1 = f(new Bar) // warn
  def t2: Any = f(new Bar) // no warn (because of expected type `Any`, see scala/scala#9452)
}

// from scala/scala#4401 / scala/scala#11053. note that inference improved, there's no `Any` inferred in either test1/2/3
class pr4401 {
  trait Binary[A, B]
  type Unary[A] = Binary[A, A]
  def f[F[A], A](f: F[A]): f.type = f

  def test1(u: Unary[Any]) = f(u)
  def test2(u: Binary[Any, Any]) = f(u)
  def test3 = {
    implicit def b2u[A, B](b: Binary[A, B]): List[Int] = ???
    val b: Binary[Any, Any] = null
    f(b)
  }

  def check1: Unary[Any] = test1(???)
  def check2: Binary[Any,Any] = test2(???)
  def check3: Binary[Any, Any] = test3
}
