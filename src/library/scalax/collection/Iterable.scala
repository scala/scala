/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $


package scalax.collection

import generic._

/** Collection classes mixing in this class provide a method
 *  <code>elements</code> which returns an iterator over all the
 *  elements contained in the collection.
 *
 *  @note If a collection has a known <code>size</code>, it should also sub-type <code>SizedIterable</code>.
 *
 *  @author  Matthias Zenger
 *  @autor   Martin Odersky
 *  @owner   Martin Odersky
 *  @version 2.8
 */
trait Iterable[+A] extends covariant.IterableTemplate[Iterable, A] { self =>

  /** Creates a view of this iterable @see Iterable.View
  def view: View[Iterable, A] = new View[Iterable, A] { // !!! Martin: We should maybe infer the type parameters here?
    val origin: self.type = self
    val elements: Iterator[A] = self.elements
  }
   */
}

/** Various utilities for instances of <a href="Iterable.html">Iterable</a>.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 */
object Iterable extends covariant.IterableFactory[Iterable] {

  /** The empty iterable */
  val empty: Iterable[Nothing] = null // !!!

  class OrderedIterableOps[A](seq: Iterable[A], cmp: Ordering[A]) {
    def min: A = {
      require(!seq.isEmpty, "min(<empty>)")
      var acc = seq.elements.next
      for (x <- seq)
        if (cmp.lt(x, acc)) acc = x
      acc
    }
    def max: A = {
      require(!seq.isEmpty, "max(<empty>)")
      var acc = seq.elements.next
      for (x <- seq)
        if (cmp.gt(x, acc)) acc = x
      acc
    }
  }

  class NumericIterableOps[A](seq: Iterable[A], num: Numeric[A]) {
    def sum: A = {
      var acc = num.zero
      for (x <- seq) acc = num.plus(acc, x)
      acc
    }
    def product: A = {
      var acc = num.one
      for (x <- seq) acc = num.times(acc, x)
      acc
    }
  }

  class IterableIterableOps[C[+B] <: Iterable[B], A](self: C[Iterable[A]]) {
    def flatten: C[A] = {
      val b = self.newBuilder[A].asInstanceOf[Builder[C, A]]
      for (xs <- self)
        b ++= xs
      b.result
    }
  }

  class PairCollectionOps[C[+B] <: Iterable[B], A1, A2](self: C[(A1, A2)]) {
    def unzip: (C[A1], C[A2]) = {
      val as = self.newBuilder[A1].asInstanceOf[Builder[C, A1]]
      val bs = self.newBuilder[A2].asInstanceOf[Builder[C, A2]]
      for ((a, b) <- self) {
        as += a
        bs += b
      }
      (as.result, bs.result)
    }
  }

  implicit def orderedIterableWrapper[A](seq: Iterable[A])(implicit cmp: Ordering[A]) =
    new OrderedIterableOps(seq, cmp)
  implicit def numericIterableWrapper[A](seq: Iterable[A])(implicit num: Numeric[A]) =
    new NumericIterableOps(seq, num)
  implicit def iterableIterableWrapper[C[+B] <: Iterable[B], A](seq: C[Iterable[A]]) =
    new IterableIterableOps[C, A](seq)
  implicit def pairCollectionWrapper[C[+B] <: Iterable[B], A1, A2](seq: C[(A1, A2)]) =
    new PairCollectionOps[C, A1, A2](seq)

  type View[+UC[+B] <: Sequence[B], +A] = covariant.IterableView[UC, A]

  /** @deprecated use View instead
   */
  @deprecated type Projection[+A] = View[C, A] forSome { type C[B] <: Iterable[B] }

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

}

