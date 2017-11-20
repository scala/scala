package strawman.collection

import scala.annotation.unchecked.uncheckedVariance

/**
  * Multiset whose elements are sorted
  * @tparam A Type of elements
  */
trait SortedMultiSet[A]
  extends MultiSet[A]
    with SortedMultiSetOps[A, SortedMultiSet, SortedMultiSet[A]] {

  def unordered: MultiSet[A] = this

}

trait SortedMultiSetOps[A, +CC[X] <: MultiSet[X], +C <: Iterable[A]]
  extends MultiSetOps[A, MultiSet, C]
    with SortedOps[A, C] {

  def iterableFactory: IterableFactoryLike[MultiSet] = MultiSet

  def sortedIterableFactory: SortedIterableFactory[CC]

  protected[this] def sortedFromIterable[B : Ordering](it: Iterable[B]): CC[B]
  protected[this] def sortedFromOccurrences[B : Ordering](it: Iterable[(B, Int)]): CC[B] =
    sortedFromIterable(it.flatMap { case (b, n) => View.Fill(n)(b) })

  /** `this` sorted multiset upcasted to an unsorted multiset */
  def unordered: MultiSet[A]

  def occurrences: SortedMap[A, Int]

  /**
    * Creates an iterator that contains all values from this collection
    * greater than or equal to `start` according to the ordering of
    * this collection. x.iteratorFrom(y) is equivalent to but will usually
    * be more efficient than x.from(y).iterator
    *
    * @param start The lower-bound (inclusive) of the iterator
    */
  def iteratorFrom(start: A): Iterator[A] =
    occurrences.iteratorFrom(start).flatMap { case (elem, n) => View.Fill(n)(elem) }

  def firstKey: A = head
  def lastKey: A = last

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

  /** Builds a new sorted multiset by applying a function to all elements of this sorted multiset.
    *
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new collection resulting from applying the given function
    *                `f` to each element of this sorted multiset and collecting the results.
    */
  def map[B : Ordering](f: A => B): CC[B] = sortedFromIterable(View.Map(toIterable, f))

  /**
    * Builds a new sorted multiset by applying a function to all pairs of element and its
    * number of occurrences.
    *
    * @param f  the function to apply
    * @tparam B the element type of the returned collection
    * @return   a new collection resulting from applying the given function
    *           `f` to each pair of element and its number of occurrences of this
    *           sorted multiset and collecting the results.
    */
  def mapOccurrences[B : Ordering](f: ((A, Int)) => (B, Int)): CC[B] =
    sortedFromOccurrences(View.Map(occurrences, f))

  /**
    * Builds a new collection by applying a function to all elements of this sorted
    * multiset and using the elements of the resulting collections.
    *
    * @param f      the function to apply to each element.
    * @tparam B     the element type of the returned collection.
    * @return a new collection resulting from applying the given function `f` to
    *         each element of this sorted multiset and concatenating the results.
    */
  def flatMap[B : Ordering](f: A => IterableOnce[B]): CC[B] = sortedFromIterable(View.FlatMap(toIterable, f))

  /**
    * Builds a new collection by applying a function to all pairs of element and
    * its number of occurrences of this sorted multiset and using the elements of
    * the resulting collections.
    *
    * @param f      the function to apply to each element.
    * @tparam B     the element type of the returned collection.
    * @return a new collection resulting from applying the given function `f` to
    *         each pair of element and its number of occurrences of this sorted
    *         multiset and concatenating the results.
    */
  def flatMapOccurrences[B : Ordering](f: ((A, Int)) => IterableOnce[(B, Int)]): CC[B] =
    sortedFromOccurrences(View.FlatMap(occurrences, f))

  /**
    * Returns a sorted multiset formed from this sorted multiset and another iterable
    * collection, by combining corresponding elements in pairs.
    * @param that The iterable providing the second half of each result pair
    * @param ev The ordering instance for type `B`
    * @tparam B the type of the second half of the returned pairs
    * @return a new sorted multiset containing pairs consisting of corresponding elements
    *         of this sorted multiset and `that`. The length of the returned collection
    *         is the minimum of the lengths of `this` and `that`
    */
  def zip[B](that: Iterable[B])(implicit ev: Ordering[B]): CC[(A @uncheckedVariance, B)] = // sound bcs of VarianceNote
    sortedFromIterable(View.Zip(toIterable, that))

  /**
    * @return a new collection resulting from applying the given partial
    *         function `pf` to each element on which it is defined and
    *         collecting the results
    * @param pf the partial function which filters and map this sorted multiset
    * @tparam B the element type of the returned collection
    */
  def collect[B : Ordering](pf: scala.PartialFunction[A, B]): CC[B] = flatMap(a =>
    if (pf.isDefinedAt(a)) View.Single(pf(a))
    else View.Empty
  )

  // --- Override return type of methods that return an unsorted MultiSet

  override def zipWithIndex: CC[(A, Int)] =
    sortedFromIterable(View.ZipWithIndex(toIterable))

  override def concat(that: Iterable[A]): CC[A] =
    sortedFromIterable(View.Concat(toIterable, that))

  override def concatOccurrences(that: Iterable[(A, Int)]): CC[A] =
    sortedFromOccurrences(View.Concat(occurrences, that))

}

object SortedMultiSet extends SortedIterableFactory.Delegate(immutable.SortedMultiSet)
