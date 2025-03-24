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

package scala.collection

import scala.annotation.{implicitNotFound, nowarn}
import scala.annotation.unchecked.uncheckedVariance

/** Base type of sorted sets */
trait SortedSet[A] extends Set[A]
    with SortedSetOps[A, SortedSet, SortedSet[A]]
    with SortedSetFactoryDefaults[A, SortedSet, Set] {

  def unsorted: Set[A] = this

  def sortedIterableFactory: SortedIterableFactory[SortedSet] = SortedSet

  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected[this] def stringPrefix: String = "SortedSet"

  override def equals(that: Any): Boolean = that match {
    case _ if this eq that.asInstanceOf[AnyRef] => true
    case ss: SortedSet[A @unchecked] if ss.ordering == this.ordering =>
      (ss canEqual this) &&
        (this.size == ss.size) && {
        val i1 = this.iterator
        val i2 = ss.iterator
        var allEqual = true
        while (allEqual && i1.hasNext)
          allEqual = ordering.equiv(i1.next(), i2.next())
        allEqual
      }
    case _ =>
      super.equals(that)
  }

}

trait SortedSetOps[A, +CC[X] <: SortedSet[X], +C <: SortedSetOps[A, CC, C]]
  extends SetOps[A, Set, C]
     with SortedOps[A, C] {

  /** The companion object of this sorted set, providing various factory methods.
    *
    * @note When implementing a custom collection type and refining `CC` to the new type, this
    *       method needs to be overridden to return a factory for the new type (the compiler will
    *       issue an error otherwise).
    */
  def sortedIterableFactory: SortedIterableFactory[CC]

  def unsorted: Set[A]

  /**
    * Creates an iterator that contains all values from this collection
    * greater than or equal to `start` according to the ordering of
    * this collection. x.iteratorFrom(y) is equivalent to but will usually
    * be more efficient than x.from(y).iterator
    *
    * @param start The lower-bound (inclusive) of the iterator
    */
  def iteratorFrom(start: A): Iterator[A]
  
  @deprecated("Use `iteratorFrom` instead.", "2.13.0")
  @`inline` def keysIteratorFrom(start: A): Iterator[A] = iteratorFrom(start)

  def firstKey: A = head
  def lastKey: A = last

  /** Find the smallest element larger than or equal to a given key.
    * @param key The given key.
    * @return `None` if there is no such node.
    */
  def minAfter(key: A): Option[A] = rangeFrom(key).headOption

  /** Find the largest element less than a given key.
    * @param key The given key.
    * @return `None` if there is no such node.
    */
  def maxBefore(key: A): Option[A] = rangeUntil(key).lastOption

  override def min[B >: A](implicit ord: Ordering[B]): A =
    if (isEmpty) throw new UnsupportedOperationException("empty.min")
    else if (ord == ordering) head
    else if (ord isReverseOf ordering) last
    else super.min[B] // need the type annotation for it to infer the correct implicit

  override def max[B >: A](implicit ord: Ordering[B]): A =
    if (isEmpty) throw new UnsupportedOperationException("empty.max")
    else if (ord == ordering) last
    else if (ord isReverseOf ordering) head
    else super.max[B] // need the type annotation for it to infer the correct implicit

  def rangeTo(to: A): C = {
    val i = rangeFrom(to).iterator
    if (i.isEmpty) return coll
    val next = i.next()
    if (ordering.compare(next, to) == 0)
      if (i.isEmpty) coll
      else rangeUntil(i.next())
    else
      rangeUntil(next)
  }

  /** Builds a new sorted collection by applying a function to all elements of this $coll.
    *
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new $coll resulting from applying the given function
    *                `f` to each element of this $coll and collecting the results.
    */
  def map[B](f: A => B)(implicit @implicitNotFound(SortedSetOps.ordMsg) ev: Ordering[B]): CC[B] =
    sortedIterableFactory.from(new View.Map(this, f))

  /** Builds a new sorted collection by applying a function to all elements of this $coll
    *  and using the elements of the resulting collections.
    *
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new $coll resulting from applying the given collection-valued function
    *                `f` to each element of this $coll and concatenating the results.
    */
  def flatMap[B](f: A => IterableOnce[B])(implicit @implicitNotFound(SortedSetOps.ordMsg) ev: Ordering[B]): CC[B] =
    sortedIterableFactory.from(new View.FlatMap(this, f))

  /** Returns a $coll formed from this $coll and another iterable collection
    *  by combining corresponding elements in pairs.
    *  If one of the two collections is longer than the other, its remaining elements are ignored.
    *
    *  @param   that  The iterable providing the second half of each result pair
    *  @tparam  B     the type of the second half of the returned pairs
    *  @return        a new $coll containing pairs consisting of corresponding elements of this $coll and `that`.
    *                 The length of the returned collection is the minimum of the lengths of this $coll and `that`.
    */
  def zip[B](that: IterableOnce[B])(implicit @implicitNotFound(SortedSetOps.zipOrdMsg) ev: Ordering[(A @uncheckedVariance, B)]): CC[(A @uncheckedVariance, B)] = // sound bcs of VarianceNote
    sortedIterableFactory.from(that match {
      case that: Iterable[B] => new View.Zip(this, that)
      case _ => iterator.zip(that)
    })

  /** Builds a new sorted collection by applying a partial function to all elements of this $coll
    *  on which the function is defined.
    *
    *  @param pf     the partial function which filters and maps the $coll.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new $coll resulting from applying the given partial function
    *                `pf` to each element on which it is defined and collecting the results.
    *                The order of the elements is preserved.
    */
  def collect[B](pf: scala.PartialFunction[A, B])(implicit @implicitNotFound(SortedSetOps.ordMsg) ev: Ordering[B]): CC[B] =
    sortedIterableFactory.from(new View.Collect(this, pf))
}

object SortedSetOps {
  private[collection] final val ordMsg = "No implicit Ordering[${B}] found to build a SortedSet[${B}]. You may want to upcast to a Set[${A}] first by calling `unsorted`."
  private[collection] final val zipOrdMsg = "No implicit Ordering[${B}] found to build a SortedSet[(${A}, ${B})]. You may want to upcast to a Set[${A}] first by calling `unsorted`."

  /** Specialize `WithFilter` for sorted collections
    *
    * @define coll sorted collection
    */
  class WithFilter[+A, +IterableCC[_], +CC[X] <: SortedSet[X]](
    self: SortedSetOps[A, CC, _] with IterableOps[A, IterableCC, _],
    p: A => Boolean
  ) extends IterableOps.WithFilter[A, IterableCC](self, p) {

    def map[B : Ordering](f: A => B): CC[B] =
      self.sortedIterableFactory.from(new View.Map(filtered, f))

    def flatMap[B : Ordering](f: A => IterableOnce[B]): CC[B] =
      self.sortedIterableFactory.from(new View.FlatMap(filtered, f))

    override def withFilter(q: A => Boolean): WithFilter[A, IterableCC, CC] =
      new WithFilter[A, IterableCC, CC](self, (a: A) => p(a) && q(a))
  }

}

@SerialVersionUID(3L)
object SortedSet extends SortedIterableFactory.Delegate[SortedSet](immutable.SortedSet)

