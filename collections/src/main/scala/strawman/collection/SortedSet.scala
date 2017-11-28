package strawman.collection

import scala.{Boolean, Ordering, `inline`, None, Option, Some}
import scala.annotation.unchecked.uncheckedVariance

/** Base type of sorted sets */
trait SortedSet[A] extends Set[A] with SortedSetOps[A, SortedSet, SortedSet[A]]

trait SortedSetOps[A, +CC[X] <: SortedSet[X], +C <: SortedSetOps[A, CC, C]]
  extends SetOps[A, Set, C]
     with SortedOps[A, C] {

  def sortedIterableFactory: SortedIterableFactory[CC]

  protected[this] def sortedFromIterable[B: Ordering](it: Iterable[B]): CC[B]

  /**
    * Creates an iterator that contains all values from this collection
    * greater than or equal to `start` according to the ordering of
    * this collection. x.iteratorFrom(y) is equivalent to but will usually
    * be more efficient than x.from(y).iterator
    *
    * @param start The lower-bound (inclusive) of the iterator
    */
  def iteratorFrom(start: A): Iterator[A]

  def firstKey: A = head
  def lastKey: A = last

  /** Find the smallest element larger than or equal to a given key.
    * @param key The given key.
    * @return `None` if there is no such node.
    */
  def minAfter(key: A): Option[A] = from(key).headOption

  /** Find the largest element less than a given key.
    * @param key The given key.
    * @return `None` if there is no such node.
    */
  def maxBefore(key: A): Option[A] = until(key).lastOption

  def rangeTo(to: A): C = {
    val i = from(to).iterator()
    if (i.isEmpty) return coll
    val next = i.next()
    if (ordering.compare(next, to) == 0)
      if (i.isEmpty) coll
      else until(i.next())
    else
      until(next)
  }

  override def withFilter(p: A => Boolean): SortedWithFilter = new SortedWithFilter(p)

  /** Specialize `WithFilter` for sorted collections
    *
    * @define coll sorted collection
    */
  class SortedWithFilter(p: A => Boolean) extends WithFilter(p) {

    def map[B : Ordering](f: A => B): CC[B] = sortedIterableFactory.from(View.Map(filtered, f))

    def flatMap[B : Ordering](f: A => IterableOnce[B]): CC[B] = sortedIterableFactory.from(View.FlatMap(filtered, f))

    override def withFilter(q: A => Boolean): SortedWithFilter = new SortedWithFilter(a => p(a) && q(a))

  }

  /** Map */
  def map[B : Ordering](f: A => B): CC[B] = sortedFromIterable(View.Map(toIterable, f))

  /** Flatmap */
  def flatMap[B : Ordering](f: A => IterableOnce[B]): CC[B] = sortedFromIterable(View.FlatMap(toIterable, f))

  /** Zip. Interesting because it requires to align to source collections. */
  def zip[B](that: Iterable[B])(implicit ev: Ordering[(A @uncheckedVariance, B)]): CC[(A @uncheckedVariance, B)] = // sound bcs of VarianceNote
    sortedFromIterable(View.Zip(toIterable, that))

  def collect[B: Ordering](pf: scala.PartialFunction[A, B]): CC[B] = flatMap(a =>
    if (pf.isDefinedAt(a)) View.Single(pf(a))
    else View.Empty
  )
}

object SortedSet extends SortedIterableFactory.Delegate[SortedSet](immutable.SortedSet)
