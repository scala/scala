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

/** <p>
 *    A template trait for iterable collections.
 *  </p>
 *  <p>
 *    Collection classes mixing in this trait provide a method
 *    <code>iterator</code> which returns an iterator over all the
 *    elements contained in the collection. They also provide a method
 *    <code>newBuilder</code> which creates a builder for collections
 *    of the same kind.
 *  </p>
 *  <p>
 *    This trait implements <code>Traversable</code>'s <code>foreach</code>
 *    method by stepping through all elements. Subclasses of <code>Iterable</code>
 *    should re-implement <code>foreach</code> with something more efficient,
 *    if possible.
 *  </p>
 *  <p>
 *    This trait adds methods <code>iterator</code>, <code>zip</code>,
 *    <code>zipAll</code>, <code>zipWithIndex</code>, <code>sameElements</code>,
 *    <code>takeRight</code>, <code>dropRight</code> to the methods inherited
 *    from trait <code>Traversable</code>.
 *  </p>
 *
 *  @author Martin Odersky
 *  @version 2.8
 */
trait Iterable[+A] extends Traversable[A]
                      with TraversableClass[A, Iterable]
                      with IterableTemplate[A, Iterable[A]] {
   override def companion: Companion[Iterable] = Iterable

  /* The following methods are inherited from trait IterableTemplate
   *
  override def iterator: Iterator[A]
  override def takeRight(n: Int): Iterable[A]
  override def dropRight(n: Int): Iterable[A]
  override def sameElements[B >: A](that: Iterable[B]): Boolean
  override def view
  override def view(from: Int, until: Int)
  */

}

/** Factory methods and utilities for instances of type Traversable */
object Iterable extends TraversableFactory[Iterable] {

  implicit def builderFactory[A]: BuilderFactory[A, Iterable[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Iterable[A]] = immutable.Iterable.newBuilder[A]

  /** The minimum element of a non-empty sequence of ordered elements */
  @deprecated("use seq.min instead")
  def min[A <% Ordered[A]](seq: Iterable[A]): A = {
    val xs = seq.iterator
    if (!xs.hasNext) throw new IllegalArgumentException("min(<empty>)")
    var min = xs.next
    while (xs.hasNext) {
      val x = xs.next
      if (x < min) min = x
    }
    min
  }

  /** The maximum element of a non-empty sequence of ordered elements */
  @deprecated("use seq.max iConstead")
  def max[A <% Ordered[A]](seq: Iterable[A]): A = {
    val xs = seq.iterator
    if (!xs.hasNext) throw new IllegalArgumentException("max(<empty>)")
    var max = xs.next
    while (xs.hasNext) {
      val x = xs.next
      if (max < x) max = x
    }
    max
  }

  @deprecated("use View instead") type Projection[A] = IterableView[A, Coll]
}
