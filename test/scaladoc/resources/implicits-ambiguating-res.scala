/**
 *  Test scaladoc implicits distinguishing -- suppress all members by implicit conversion that are shadowed by the
 *  class' own members
 *
 *  {{{
 *     scala> class A { def foo(t: String) = 4 }
 *     defined class A
 *
 *     scala> class B { def foo(t: Any) = 5 }
 *     defined class B
 *
 *     scala> implicit def AtoB(a:A) = new B
 *     AtoB: (a: A)B
 *
 *     scala> val a = new A
 *     a: A = A@28f553e3
 *
 *     scala> a.foo("T")
 *     res1: Int = 4
 *
 *     scala> a.foo(4)
 *     res2: Int = 5
 *  }}}
 */
package scala.test.scaladoc.implicits.ambiguating
import language.implicitConversions // according to SIP18

/** - conv1-5 should be ambiguous
 *  - conv6-7 should not be ambiguous
 *  - conv8 should be ambiguous
 *  - conv9 should be ambiguous
 *  - conv10 and conv11 should not be ambiguous */
class A[T]
/** conv1-9 should be the same, conv10 should be ambiguous, conv11 should be okay */
class B extends A[Int]
/** conv1-9 should be the same, conv10 and conv11 should not be ambiguous */
class C extends A[Double]
    /** conv1-9 should be the same, conv10 should not be ambiguous while conv11 should be ambiguous */
class D extends A[AnyRef]

class X[T] {
  def conv1: AnyRef = ???
  def conv2: T = ???
  def conv3(l: Int): AnyRef = ???
  def conv4(l: AnyRef): AnyRef = ???
  def conv5(l: AnyRef): String = ???
  def conv6(l: String)(m: String): AnyRef = ???
  def conv7(l: AnyRef)(m: AnyRef): AnyRef = ???
  def conv8(l: AnyRef): AnyRef = ???
  def conv9(l: String): AnyRef = ???
  def conv10(l: T): T = ???
  def conv11(l: T): T = ???
}

class Z[T] {
  def conv1: AnyRef = ???
  def conv2: T = ???
  def conv3(p: Int): AnyRef = ???
  def conv4(p: AnyRef): String = ???
  def conv5(p: AnyRef): AnyRef = ???
  def conv6(p: String, q: String): AnyRef = ???
  def conv7(p: AnyRef, q: AnyRef): AnyRef = ???
  def conv8(p: String): AnyRef = ???
  def conv9(p: AnyRef): AnyRef = ???
  def conv10(p: Int): T = ???
  def conv11(p: String): T = ???
}

object A {
  implicit def AtoX[T](a: A[T]) = new X[T]
  implicit def AtoZ[T](a: A[T]) = new Z[T]
}
