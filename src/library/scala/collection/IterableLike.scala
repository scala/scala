/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection
import generic._
import immutable.{List, Stream}
import annotation.unchecked.uncheckedVariance

/** A template trait for iterable collections of type `Iterable[A]`.
 *  $iterableInfo
 *  @tparam A    the element type of the collection
 *  @tparam Repr the type of the actual collection containing the elements.
 *  @define iterableInfo
 *    This is a base trait for all scala collections that define an `iterator`
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
 *
 *  @define Coll Iterable
 *  @define coll iterable collection
 *  @define zipthatinfo the class of the returned collection. Where possible, `That` is
 *    the same class as the current collection class `Repr`, but this
 *    depends on the element type `(A1, B)` being admissible for that class,
 *    which means that an implicit instance of type `CanBuildFrom[Repr, (A1, B), That]`.
 *    is found.
 *  @define zipbfinfo  an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `(A1, B)`.
 */
trait IterableLike[+A, +Repr] extends Equals with TraversableLike[A, Repr] {
self =>

  override protected[this] def thisCollection: Iterable[A] = this.asInstanceOf[Iterable[A]]
  override protected[this] def toCollection(repr: Repr): Iterable[A] = repr.asInstanceOf[Iterable[A]]

  /** Creates a new iterator over all elements contained in this
   *  iterable object.
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
   */
  def foreach[U](f: A => U): Unit =
    iterator.foreach(f)

  override /*TraversableLike*/ def forall(p: A => Boolean): Boolean =
    iterator.forall(p)
  override /*TraversableLike*/ def exists(p: A => Boolean): Boolean =
    iterator.exists(p)
  override /*TraversableLike*/ def find(p: A => Boolean): Option[A] =
    iterator.find(p)
/*
  override /*TraversableLike*/ def mapFind[B](f: A => Option[B]): Option[B] =
    iterator.mapFind(f)
*/
  override /*TraversableLike*/ def isEmpty: Boolean =
    !iterator.hasNext
  override /*TraversableLike*/ def foldRight[B](z: B)(op: (A, B) => B): B =
    iterator.foldRight(z)(op)
  override /*TraversableLike*/ def reduceRight[B >: A](op: (A, B) => B): B =
    iterator.reduceRight(op)
  override /*TraversableLike*/ def toIterable: Iterable[A] =
    thisCollection

  override /*TraversableLike*/ def head: A =
    if (isEmpty) throw new NoSuchElementException
    else iterator.next

  override /*TraversableLike*/ def take(n: Int): Repr = {
    val b = newBuilder
    var i = 0
    val it = iterator
    while (i < n && it.hasNext) {
      b += it.next
      i += 1
    }
    b.result
  }

  override /*TraversableLike*/ def slice(from: Int, until: Int): Repr = {
    val b = newBuilder
    var i = from
    val it = iterator drop from
    while (i < until && it.hasNext) {
      b += it.next
      i += 1
    }
    b.result
  }

  override /*TraversableLike*/ def takeWhile(p: A => Boolean): Repr = {
    val b = newBuilder
    val it = iterator
    while (it.hasNext) {
      val x = it.next
      if (!p(x)) return b.result
      b += x
    }
    b.result
  }

  /** Partitions elements in fixed size ${coll}s.
   *  @see Iterator#grouped
   *
   *  @param size the number of elements per group
   *  @return An iterator producing ${coll}s of size `size`, except the
   *          last will be truncated if the elements don't divide evenly.
   */
  def grouped(size: Int): Iterator[Repr] =
    for (xs <- iterator grouped size) yield {
      val b = newBuilder
      b ++= xs
      b.result
    }

  /** Groups elements in fixed size blocks by passing a "sliding window"
   *  over them (as opposed to partitioning them, as is done in grouped.)
   *  @see Iterator#sliding
   *
   *  @param size the number of elements per group
   *  @param step the distance between the first elements of successive
   *         groups (defaults to 1)
   *  @return An iterator producing ${coll}s of size `size`, except the
   *          last will be truncated if the elements don't divide evenly.
   */
  def sliding[B >: A](size: Int): Iterator[Repr] = sliding(size, 1)
  def sliding[B >: A](size: Int, step: Int): Iterator[Repr] =
    for (xs <- iterator.sliding(size, step)) yield {
      val b = newBuilder
      b ++= xs
      b.result
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
    val lead = this.iterator drop n
    var go = false
    for (x <- this) {
      if (lead.hasNext) lead.next
      else go = true
      if (go) b += x
    }
    b.result
  }

  /** Selects all elements except last ''n'' ones.
   *  $orderDependent
   *
   *  @param  n    The number of elements to take
   *  @return a $coll consisting of all elements of this $coll except the first `n` ones, or else the
   *          empty $coll, if this $coll has less than `n` elements.
   */
  def dropRight(n: Int): Repr = {
    val b = newBuilder
    val lead = iterator drop n
    val it = iterator
    while (lead.hasNext) {
      b += it.next
      lead.next
    }
    b.result
  }

  override /*TraversableLike*/ def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) {
    var i = start
    val end = (start + len) min xs.length
    val it = iterator
    while (i < end && it.hasNext) {
      xs(i) = it.next
      i += 1
    }
  }

  /** Returns a $coll formed from this $coll and another iterable collection
   *  by combining corresponding elements in pairs.
   *  If one of the two collections is longer than the other, its remaining elements are ignored.
   *
   *  $orderDependent
   *
   *  @param   that  The iterable providing the second half of each result pair
   *  @tparam  A1    the type of the first half of the returned pairs (this is always a supertype
   *                 of the collection's element type `A`).
   *  @tparam  B     the type of the second half of the returned pairs
   *  @tparam  That  $zipthatinfo
   *  @param   bf    $zipbfinfo
   *  @return        a new collection of type `That` containing pairs consisting of
   *                 corresponding elements of this $coll and `that`. The length
   *                 of the returned collection is the minimum of the lengths of this $coll$ and `that`.
   *
   *  @usecase def zip[B](that: Iterable[B]): $Coll[(A, B)]
   *
   *  @param   that  The iterable providing the second half of each result pair
   *  @tparam  B     the type of the second half of the returned pairs
   *  @return        a new $coll containing pairs consisting of
   *                 corresponding elements of this $coll and `that`. The length
   *                 of the returned collection is the minimum of the lengths of this $coll$ and `that`.
   */
  def zip[A1 >: A, B, That](that: Iterable[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
    val b = bf(repr)
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      b += ((these.next, those.next))
    b.result
  }

  /** Returns a $coll formed from this $coll and another iterable collection
   *  by combining corresponding elements in pairs.
   *  If one of the two collections is shorter than the other,
   *  placeholder elements are used to extend the shorter collection to the length of the longer.
   *
   *  $orderDependent
   *
   *  @param that     the iterable providing the second half of each result pair
   *  @param thisElem the element to be used to fill up the result if this $coll is shorter than `that`.
   *  @param thatElem the element to be used to fill up the result if `that` is shorter than this $coll.
   *  @return        a new collection of type `That` containing pairs consisting of
   *                 corresponding elements of this $coll and `that`. The length
   *                 of the returned collection is the maximum of the lengths of this $coll$ and `that`.
   *                 If this $coll is shorter than `that`, `thisElem` values are used to pad the result.
   *                 If `that` is shorter than this $coll, `thatElem` values are used to pad the result.
   *
   *  @usecase def zipAll[B](that: Iterable[B], thisElem: A, thatElem: B): $Coll[(A, B)]
   *
   *  @param   that  The iterable providing the second half of each result pair
   *  @param thisElem the element to be used to fill up the result if this $coll is shorter than `that`.
   *  @param thatElem the element to be used to fill up the result if `that` is shorter than this $coll.
   *  @tparam  B     the type of the second half of the returned pairs
   *  @return        a new $coll containing pairs consisting of
   *                 corresponding elements of this $coll and `that`. The length
   *                 of the returned collection is the maximum of the lengths of this $coll$ and `that`.
   *                 If this $coll is shorter than `that`, `thisElem` values are used to pad the result.
   *                 If `that` is shorter than this $coll, `thatElem` values are used to pad the result.
   */
  def zipAll[B, A1 >: A, That](that: Iterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
    val b = bf(repr)
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      b += ((these.next, those.next))
    while (these.hasNext)
      b += ((these.next, thatElem))
    while (those.hasNext)
      b += ((thisElem, those.next))
    b.result
  }

  /** Zips this $coll with its indices.
   *
   *  $orderDependent
   *
   *  @tparam  A1    the type of the first half of the returned pairs (this is always a supertype
   *                 of the collection's element type `A`).
   *  @tparam  That  the class of the returned collection. Where possible, `That` is
   *    the same class as the current collection class `Repr`, but this
   *    depends on the element type `(A1, Int)` being admissible for that class,
   *    which means that an implicit instance of type `CanBuildFrom[Repr, (A1, Int), That]`.
   *    is found.
   *  @tparam  bf    an implicit value of class `CanBuildFrom` which determines the
   *    result class `That` from the current representation type `Repr`
   *    and the new element type `(A1, Int)`.
   *  @return        A new collection of type `That` containing pairs consisting of all elements of this
   *                 $coll paired with their index. Indices start at `0`.
   *
   *  @usecase def zipWithIndex: $Coll[(A, Int)]
   *
   *  @return        A new $coll containing pairs consisting of all elements of this
   *                 $coll paired with their index. Indices start at `0`.
   *  @example
   *    `List("a", "b", "c").zipWithIndex = List(("a", 0), ("b", 1), ("c", 2))`
   *
   */
  def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[Repr, (A1, Int), That]): That = {
    val b = bf(repr)
    var i = 0
    for (x <- this) {
      b += ((x, i))
      i +=1
    }
    b.result
  }

  /** Checks if the other iterable collection contains the same elements in the same order as this $coll.
   *
   *  $orderDependent
   *  $willNotTerminateInf
   *
   *  @param that  the collection to compare with.
   *  @tparam B    the type of the elements of collection `that`.
   *  @return `true`, if both collections contain the same elements in the same order, `false` otherwise.
   *
   *  @usecase  def sameElements(that: Iterable[A]): Boolean
   *
   *  @param that  the collection to compare with.
   *  @return `true`, if both collections contain the same elements in the same order, `false` otherwise.
   */
  def sameElements[B >: A](that: Iterable[B]): Boolean = {
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

  @deprecated("use `iterator' instead")
  def elements = iterator

  @deprecated("use `head' instead") def first: A = head

  /** `None` if iterable is empty.
   */
  @deprecated("use `headOption' instead") def firstOption: Option[A] = headOption

  /**
   * returns a projection that can be used to call non-strict `filter`,
   * `map`, and `flatMap` methods that build projections
   * of the collection.
   */
  @deprecated("use `view' instead")
  def projection = view
}
