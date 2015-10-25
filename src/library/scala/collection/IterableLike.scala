/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

import generic._
import immutable.Stream

/** A template trait for iterable collections of type `Iterable[A]`.
 *  $iterableInfo
 *  @define iterableInfo
 *    This is a base trait for all $mutability Scala collections that define an `iterator`
 *    method to step through one-by-one the collection's elements.
 *    Implementations of this trait need to provide a concrete method with
 *    signature:
 *    {{{
 *       def iterator: Iterator[A]
 *    }}}
 *    They also need to provide a method `newBuilder`
 *    which creates a builder for collections of the same kind.
 *
 *    This trait implements `Iterable`'s `foreach`
 *    method by stepping through all elements using `iterator`.
 *    Subclasses should re-implement `foreach` with something more efficient,
 *    if possible.

 *    This trait adds methods `iterator`, `sameElements`,
 *    `takeRight`, `dropRight` to the methods inherited
 *    from trait <a href="../Traversable.html" target="ContentFrame">
 *    `Traversable`</a>.

 *    Note: This trait replaces every method that uses `break` in
 *    `TraversableLike` by an iterator version.
 *
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @tparam A    the element type of the collection
 *  @tparam Repr the type of the actual collection containing the elements.
 *
 *  @define Coll Iterable
 *  @define coll iterable collection
 */
trait IterableLike[+A, +Repr] extends Any with Equals with TraversableLike[A, Repr] with GenIterableLike[A, Repr] {
self =>

  override protected[this] def thisCollection: Iterable[A] = this.asInstanceOf[Iterable[A]]
  override protected[this] def toCollection(repr: Repr): Iterable[A] = repr.asInstanceOf[Iterable[A]]

  /** Creates a new iterator over all elements contained in this iterable object.
   *
   *  @return the new iterator
   */
  def iterator: Iterator[A]

  /** Applies a function `f` to all elements of this $coll.
   *
   *    Note: this method underlies the implementation of most other bulk operations.
   *    Subclasses should re-implement this method if a more efficient implementation exists.
   *
   *  @usecase def foreach(f: A => Unit): Unit
   *    @inheritdoc
   */
  def foreach[U](f: A => U): Unit =
    iterator.foreach(f)

  override /*TraversableLike*/ def forall(p: A => Boolean): Boolean =
    iterator.forall(p)
  override /*TraversableLike*/ def exists(p: A => Boolean): Boolean =
    iterator.exists(p)
  override /*TraversableLike*/ def find(p: A => Boolean): Option[A] =
    iterator.find(p)
  override /*TraversableLike*/ def isEmpty: Boolean =
    !iterator.hasNext
  override /*TraversableLike*/ def foldRight[B](z: B)(op: (A, B) => B): B =
    iterator.foldRight(z)(op)
  override /*TraversableLike*/ def reduceRight[B >: A](op: (A, B) => B): B =
    iterator.reduceRight(op)


  /** Returns this $coll as an iterable collection.
   *
   *  A new collection will not be built; lazy collections will stay lazy.
   *
   *  $willNotTerminateInf
   *  @return an `Iterable` containing all elements of this $coll.
   */
  override /*TraversableLike*/ def toIterable: Iterable[A] =
    thisCollection

  /** Returns an Iterator over the elements in this $coll.  Produces the same
   *  result as `iterator`.
   *  $willNotTerminateInf
   *  @return an Iterator containing all elements of this $coll.
   */
  @deprecatedOverriding("toIterator should stay consistent with iterator for all Iterables: override iterator instead.", "2.11.0")
  override def toIterator: Iterator[A] = iterator

  override /*TraversableLike*/ def head: A =
    iterator.next()

  override /*TraversableLike*/ def slice(from: Int, until: Int): Repr = {
    val lo = math.max(from, 0)
    val elems = until - lo
    val b = newBuilder
    if (elems <= 0) b.result()
    else {
      b.sizeHintBounded(elems, this)
      var i = 0
      val it = iterator drop lo
      while (i < elems && it.hasNext) {
        b += it.next
        i += 1
      }
      b.result()
    }
  }

  override /*TraversableLike*/ def take(n: Int): Repr = {
    val b = newBuilder

    if (n <= 0) b.result()
    else {
      b.sizeHintBounded(n, this)
      var i = 0
      val it = iterator
      while (i < n && it.hasNext) {
        b += it.next
        i += 1
      }
      b.result()
    }
  }

  override /*TraversableLike*/ def drop(n: Int): Repr = {
    val b = newBuilder
    val lo = math.max(0, n)
    b.sizeHint(this, -lo)
    var i = 0
    val it = iterator
    while (i < n && it.hasNext) {
      it.next()
      i += 1
    }
    (b ++= it).result()
  }

  override /*TraversableLike*/ def takeWhile(p: A => Boolean): Repr = {
    val b = newBuilder
    val it = iterator
    while (it.hasNext) {
      val x = it.next()
      if (!p(x)) return b.result()
      b += x
    }
    b.result()
  }

  /** Partitions elements in fixed size ${coll}s.
   *  @see [[scala.collection.Iterator]], method `grouped`
   *
   *  @param size the number of elements per group
   *  @return An iterator producing ${coll}s of size `size`, except the
   *          last will be less than size `size` if the elements don't divide evenly.
   */
  def grouped(size: Int): Iterator[Repr] =
    for (xs <- iterator grouped size) yield {
      val b = newBuilder
      b ++= xs
      b.result()
    }

  /** Groups elements in fixed size blocks by passing a "sliding window"
   *  over them (as opposed to partitioning them, as is done in grouped.)
   *  "Sliding window" step is 1 by default.
   *  @see [[scala.collection.Iterator]], method `sliding`
   *
   *  @param size the number of elements per group
   *  @return An iterator producing ${coll}s of size `size`, except the
   *          last and the only element will be truncated if there are
   *          fewer elements than size.
   */
  def sliding(size: Int): Iterator[Repr] = sliding(size, 1)

  /** Groups elements in fixed size blocks by passing a "sliding window"
   *  over them (as opposed to partitioning them, as is done in grouped.)
   *  @see [[scala.collection.Iterator]], method `sliding`
   *
   *  @param size the number of elements per group
   *  @param step the distance between the first elements of successive
   *         groups
   *  @return An iterator producing ${coll}s of size `size`, except the
   *          last and the only element will be truncated if there are
   *          fewer elements than size.
   */
  def sliding(size: Int, step: Int): Iterator[Repr] =
    for (xs <- iterator.sliding(size, step)) yield {
      val b = newBuilder
      b ++= xs
      b.result()
    }

  /** Selects last ''n'' elements.
   *  $orderDependent
   *
   *  @param n the number of elements to take
   *  @return a $coll consisting only of the last `n` elements of this $coll, or else the
   *          whole $coll, if it has less than `n` elements.
   */
  def takeRight(n: Int): Repr = {
    val b = newBuilder
    b.sizeHintBounded(n, this)
    val lead = this.iterator drop n
    val it = this.iterator
    while (lead.hasNext) {
      lead.next()
      it.next()
    }
    while (it.hasNext) b += it.next()
    b.result()
  }

  /** Selects all elements except last ''n'' ones.
   *  $orderDependent
   *
   *  @param  n    The number of elements to take
   *  @return a $coll consisting of all elements of this $coll except the last `n` ones, or else the
   *          empty $coll, if this $coll has less than `n` elements.
   */
  def dropRight(n: Int): Repr = {
    val b = newBuilder
    if (n >= 0) b.sizeHint(this, -n)
    val lead = iterator drop n
    val it = iterator
    while (lead.hasNext) {
      b += it.next
      lead.next()
    }
    b.result()
  }

  override /*TraversableLike*/ def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) {
    var i = start
    val end = (start + len) min xs.length
    val it = iterator
    while (i < end && it.hasNext) {
      xs(i) = it.next()
      i += 1
    }
  }

  def zip[A1 >: A, B, That](that: GenIterable[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
    val b = bf(repr)
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      b += ((these.next(), those.next()))
    b.result()
  }

  def zipAll[B, A1 >: A, That](that: GenIterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
    val b = bf(repr)
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      b += ((these.next(), those.next()))
    while (these.hasNext)
      b += ((these.next(), thatElem))
    while (those.hasNext)
      b += ((thisElem, those.next()))
    b.result()
  }

  def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[Repr, (A1, Int), That]): That = {
    val b = bf(repr)
    var i = 0
    for (x <- this) {
      b += ((x, i))
      i += 1
    }
    b.result()
  }

  def sameElements[B >: A](that: GenIterable[B]): Boolean = {
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      if (these.next != those.next)
        return false

    !these.hasNext && !those.hasNext
  }

  override /*TraversableLike*/ def toStream: Stream[A] = iterator.toStream

  /** Method called from equality methods, so that user-defined subclasses can
   *  refuse to be equal to other collections of the same kind.
   *  @param   that   The object with which this $coll should be compared
   *  @return  `true`, if this $coll can possibly equal `that`, `false` otherwise. The test
   *           takes into consideration only the run-time types of objects but ignores their elements.
   */
  override /*TraversableLike*/ def canEqual(that: Any) = true

  override /*TraversableLike*/ def view = new IterableView[A, Repr] {
    protected lazy val underlying = self.repr
    override def iterator = self.iterator
  }

  override /*TraversableLike*/ def view(from: Int, until: Int) = view.slice(from, until)
}
