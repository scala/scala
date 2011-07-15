/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.math

/** A trait for totally ordered data.
 *
 *  Note that since version 2006-07-24 this trait is no longer covariant
 *  in `A`.
 *
 *  It is important that the `equals` method for an instance of `Ordered[A]`
 *  be consistent with the compare method.  However,  due to limitations
 *  inherent in the type erasure semantics, there is no reasonable way to
 *  provide a default implementation of equality for instances of `Ordered[A]`.
 *  Therefore, if you need to be able to use equality on an instance of
 *  `Ordered[A]` you must provide it yourself either when inheriting or
 *  instantiating.
 *
 *  It is important that the `hashCode` method for an instance of `Ordered[A]`
 *  be consistent with the `compare` method. However, it is not possible to
 *  provide a sensible default implementation. Therefore, if you need to be
 *  able compute the hash of an instance of `Ordered[A]` you must provide it
 *  yourself either when inheriting or instantiating.
 *
 *  @author  Martin Odersky
 *  @version 1.1, 2006-07-24
 */
trait Ordered[A] extends java.lang.Comparable[A] {

  /** Result of comparing `this` with operand `that`.
   *  returns `x` where
   *  `x &lt; 0`    iff    `this &lt; that`
   *  `x == 0`     iff    `this == that`
   *  `x &gt; 0`    iff    `this &gt; that`
   */
  def compare(that: A): Int

  def <  (that: A): Boolean = (this compare that) <  0
  def >  (that: A): Boolean = (this compare that) >  0
  def <= (that: A): Boolean = (this compare that) <= 0
  def >= (that: A): Boolean = (this compare that) >= 0
  def compareTo(that: A): Int = compare(that)
}

object Ordered {
  /** Lens from `Ordering[T]` to `Ordered[T]` */
  implicit def orderingToOrdered[T](x: T)(implicit ord: Ordering[T]): Ordered[T] =
    new Ordered[T] { def compare(that: T): Int = ord.compare(x, that) }
}
