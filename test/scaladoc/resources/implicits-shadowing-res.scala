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
package scala.test.scaladoc.implicits.shadowing
import language.implicitConversions // according to SIP18

/** conv5, conv8, conv9, conv10, conv11 should be visible */
class A[T] {
  def conv1: AnyRef = ???
  def conv2: T = ???
  def conv3(l: Int): AnyRef = ???
  def conv4(l: AnyRef): AnyRef = ???
  def conv5(l: String): AnyRef = ???
  def conv6(l: AnyRef): AnyRef = ???
  def conv7(l: AnyRef): String = ???
  def conv8(l: String)(m: String): AnyRef = ???
  def conv9(l: AnyRef)(m: AnyRef): AnyRef = ???
  def conv10(l: T): T = ???
  def conv11(l: T): T = ???
}
/** conv5, conv8, conv9, conv11 should be visible */
class B extends A[Int]
/** conv5, conv8, conv9, conv10, conv11 should be visible */
class C extends A[Double]
/** conv5, conv8, conv9, conv10 should be visible */
class D extends A[AnyRef]

class Z[T] {
  def conv1: AnyRef = ???
  def conv2: T = ???
  def conv3(p: Int): AnyRef = ???
  def conv4(p: String): AnyRef = ???
  def conv5(p: AnyRef): AnyRef = ???
  def conv6(p: AnyRef): String = ???
  def conv7(p: AnyRef): AnyRef = ???
  def conv8(p: String, q: String): AnyRef = ???
  def conv9(p: AnyRef, q: AnyRef): AnyRef = ???
  def conv10(p: Int): T = ???
  def conv11(p: String): T = ???
}
object A {
  implicit def AtoZ[T](a: A[T]) = new Z[T]
}
