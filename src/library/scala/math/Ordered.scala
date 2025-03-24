/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package math

import scala.language.implicitConversions

/** A trait for data that have a single, natural ordering.  See
 *  [[scala.math.Ordering]] before using this trait for
 *  more information about whether to use [[scala.math.Ordering]] instead.
 *
 *  Classes that implement this trait can be sorted with
 *  [[scala.util.Sorting]] and can be compared with standard comparison operators
 *  (e.g. > and <).
 *
 *  Ordered should be used for data with a single, natural ordering (like
 *  integers) while Ordering allows for multiple ordering implementations.
 *  An Ordering instance will be implicitly created if necessary.
 *
 *  [[scala.math.Ordering]] is an alternative to this trait that allows multiple orderings to be
 *  defined for the same type.
 *
 *  [[scala.math.PartiallyOrdered]] is an alternative to this trait for partially ordered data.
 *
 *  For example, create a simple class that implements `Ordered` and then sort it with [[scala.util.Sorting]]:
 *  {{{
 *  case class OrderedClass(n:Int) extends Ordered[OrderedClass] {
 *  	def compare(that: OrderedClass) =  this.n - that.n
 *  }
 *
 *  val x = Array(OrderedClass(1), OrderedClass(5), OrderedClass(3))
 *  scala.util.Sorting.quickSort(x)
 *  x
 *  }}}
 *
 *  It is important that the `equals` method for an instance of `Ordered[A]` be consistent with the
 *  compare method. However, due to limitations inherent in the type erasure semantics, there is no
 *  reasonable way to provide a default implementation of equality for instances of `Ordered[A]`.
 *  Therefore, if you need to be able to use equality on an instance of `Ordered[A]` you must
 *  provide it yourself either when inheriting or instantiating.
 *
 *  It is important that the `hashCode` method for an instance of `Ordered[A]` be consistent with
 *  the `compare` method. However, it is not possible to provide a sensible default implementation.
 *  Therefore, if you need to be able compute the hash of an instance of `Ordered[A]` you must
 *  provide it yourself either when inheriting or instantiating.
 *
 *  @see [[scala.math.Ordering]], [[scala.math.PartiallyOrdered]]
 */
trait Ordered[A] extends Any with java.lang.Comparable[A] {

  /** Result of comparing `this` with operand `that`.
   *
   * Implement this method to determine how instances of A will be sorted.
   *
   * Returns `x` where:
   *
   *   - `x < 0` when `this < that`
   *
   *   - `x == 0` when `this == that`
   *
   *   - `x > 0` when  `this > that`
   *
   */
  def compare(that: A): Int

  /** Returns true if `this` is less than `that`
    */
  def <  (that: A): Boolean = (this compare that) <  0

  /** Returns true if `this` is greater than `that`.
    */
  def >  (that: A): Boolean = (this compare that) >  0

  /** Returns true if `this` is less than or equal to `that`.
    */
  def <= (that: A): Boolean = (this compare that) <= 0

  /** Returns true if `this` is greater than or equal to `that`.
    */
  def >= (that: A): Boolean = (this compare that) >= 0

  /** Result of comparing `this` with operand `that`.
    */
  def compareTo(that: A): Int = compare(that)
}

object Ordered {
  /** Lens from `Ordering[T]` to `Ordered[T]` */
  implicit def orderingToOrdered[T](x: T)(implicit ord: Ordering[T]): Ordered[T] =
    new Ordered[T] { def compare(that: T): Int = ord.compare(x, that) }
}
