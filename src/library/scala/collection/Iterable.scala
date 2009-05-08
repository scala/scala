/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $

package scala.collection

import util.control.Breaks._
// import immutable.Stream
import generic._

/** A template trait for iterable collections.
 *
 *  Collection classes mixing in this trait provide a method
 *  <code>elements</code> which returns an iterator over all the
 *  elements contained in the collection. They also provide a method `newBuilder`
 *  which creates a builder for collections of the same kind.
 *
 *  This trait implements Traversable's `foreach` method by stepping through
 *  all elements. Subclasses of `Iterable` should re-implement `foreach` with
 *  something more efficient, if possible.
 *
 *  This trait adds methods `elements`, `zip`, `zipAll`, `zipWithIndex`, `sameElements`,
 *  `takeRight`, `dropRight` to the methods inherited from trait `Traversable`.
 *
 *  @author Martin Odersky
 *  @version 2.8
 */
trait Iterable[+A] extends Traversable[A] with IterableTemplate[A, Iterable[A]] {
  override protected[this] def newBuilder = Iterable.newBuilder
  override def traversableBuilder[B]: Builder[B, Iterable[B], Any] = Iterable.newBuilder[B]

  /* The following methods are inherited from trait IterableTemplate
   *
  override def elements: Iterator[A]
  override def takeRight(n: Int): Iterable[A]
  override def dropRight(n: Int): Iterable[A]
  override def sameElements[B >: A](that: Iterable[B]): Boolean
  override def view
  override def view(from: Int, until: Int)
  */
}

/** Factory methods and utilities for instances of type Traversable */
object Iterable extends TraversableFactory[Iterable] {

  type Coll	= Iterable[_]
  implicit def builderFactory[A]: BuilderFactory[A, Iterable[A], Coll] = new BuilderFactory[A, Iterable[A], Coll] { def apply(from: Coll) = from.traversableBuilder[A] }
  def newBuilder[A]: Builder[A, Iterable[A], Any] = immutable.Iterable.newBuilder[A]

  def fromOld[A](it: scala.Iterable[A]): Iterable[A] = new Iterable[A] {
    def elements: Iterator[A] = Iterator.fromOld(it.elements)
  }

  def toOld[A](it: Iterable[A]): scala.Iterable[A] = new scala.Iterable[A] {
    def elements: scala.Iterator[A] = Iterator.toOld(it.elements)
  }

  /** The minimum element of a non-empty sequence of ordered elements
   *  @deprecated use seq.min instead
   */
  @deprecated def min[A <% Ordered[A]](seq: Iterable[A]): A = {
    val xs = seq.elements
    if (!xs.hasNext) throw new IllegalArgumentException("min(<empty>)")
    var min = xs.next
    while (xs.hasNext) {
      val x = xs.next
      if (x < min) min = x
    }
    min
  }

  /** The maximum element of a non-empty sequence of ordered elements
   *  @deprecated use seq.max iConstead
   */
  @deprecated def max[A <% Ordered[A]](seq: Iterable[A]): A = {
    val xs = seq.elements
    if (!xs.hasNext) throw new IllegalArgumentException("max(<empty>)")
    var max = xs.next
    while (xs.hasNext) {
      val x = xs.next
      if (max < x) max = x
    }
    max
  }

  /** @deprecated use View instead
   */
  @deprecated type Projection[A] = IterableView[A, Coll]
}
