package scala.collection

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.DefaultSerializationProxy
import scala.language.higherKinds

/** Base type of sorted sets */
trait SortedSet[A] extends Set[A] with SortedSetOps[A, SortedSet, SortedSet[A]] {
  def unsorted: Set[A] = this

  override protected def fromSpecificIterable(coll: Iterable[A] @uncheckedVariance): SortedIterableCC[A] @uncheckedVariance = sortedIterableFactory.from(coll)
  override protected def newSpecificBuilder: mutable.Builder[A, SortedIterableCC[A]] @uncheckedVariance = sortedIterableFactory.newBuilder[A]

  /**
    * @note This operation '''has''' to be overridden by concrete collection classes to effectively
    *       return a `SortedIterableFactory[SortedIterableCC]`. The implementation in `SortedSet` only returns
    *       a `SortedIterableFactory[SortedSet]`, but the compiler will '''not''' throw an error if the
    *       effective `SortedIterableCC` type constructor is more specific than `SortedSet`.
    *
    * @return The factory of this collection.
    */
  def sortedIterableFactory: SortedIterableFactory[SortedIterableCC] = SortedSet

  override def empty: SortedIterableCC[A] = sortedIterableFactory.empty

  override protected[this] def writeReplace(): AnyRef = new DefaultSerializationProxy(sortedIterableFactory.evidenceIterableFactory[A], this)
}

trait SortedSetOps[A, +CC[X] <: SortedSetOps[X, CC, _] with SortedSet[X], +C <: SortedSetOps[A, CC, C] with CC[A]]
  extends InvariantSetOps[A, CC, C]
     with SortedOps[A, C] {

  /**
    * Type alias to `CC`. It is used to provide a default implementation of the `fromSpecificIterable`
    * and `newSpecificBuilder` operations.
    *
    * Due to the `@uncheckedVariance` annotation, usage of this type member can be unsound and is
    * therefore not recommended.
    */
  protected type SortedIterableCC[X] = CC[X] @uncheckedVariance

  def sortedIterableFactory: SortedIterableFactory[SortedIterableCC]

  /** Similar to `fromSpecificIterable`, but for a (possibly) different type of element.
    * Note that the return type is now `CC[B]` aka `SortedIterableCC[B]` rather than `IterableCC[B]`.
    */
  @`inline` final protected def sortedFromIterable[B: Ordering](it: Iterable[B]): CC[B] = sortedIterableFactory.from(it)

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

  override def withFilter(p: A => Boolean): SortedSetOps.WithFilter[A, IterableCC, CC] = new SortedSetOps.WithFilter[A, IterableCC, CC](this, p)

  /** Builds a new sorted collection by applying a function to all elements of this $coll.
    *
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new $coll resulting from applying the given function
    *                `f` to each element of this $coll and collecting the results.
    */
  def map[B : Ordering](f: A => B): CC[B] = sortedFromIterable(new View.Map(toIterable, f))

  /** Builds a new sorted collection by applying a function to all elements of this $coll
    *  and using the elements of the resulting collections.
    *
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new $coll resulting from applying the given collection-valued function
    *                `f` to each element of this $coll and concatenating the results.
    */
  def flatMap[B : Ordering](f: A => IterableOnce[B]): CC[B] = sortedFromIterable(new View.FlatMap(toIterable, f))

  /** Returns a $coll formed from this $coll and another iterable collection
    *  by combining corresponding elements in pairs.
    *  If one of the two collections is longer than the other, its remaining elements are ignored.
    *
    *  @param   that  The iterable providing the second half of each result pair
    *  @tparam  B     the type of the second half of the returned pairs
    *  @return        a new $coll containing pairs consisting of corresponding elements of this $coll and `that`.
    *                 The length of the returned collection is the minimum of the lengths of this $coll and `that`.
    */
  def zip[B](that: Iterable[B])(implicit ev: Ordering[(A @uncheckedVariance, B)]): CC[(A @uncheckedVariance, B)] = // sound bcs of VarianceNote
    sortedFromIterable(new View.Zip(toIterable, that))

  /** Builds a new sorted collection by applying a partial function to all elements of this $coll
    *  on which the function is defined.
    *
    *  @param pf     the partial function which filters and maps the $coll.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new $coll resulting from applying the given partial function
    *                `pf` to each element on which it is defined and collecting the results.
    *                The order of the elements is preserved.
    */
  def collect[B: Ordering](pf: scala.PartialFunction[A, B]): CC[B] = flatMap(a =>
    if (pf.isDefinedAt(a)) new View.Single(pf(a))
    else View.Empty
  )
}

object SortedSetOps {

  /** Specialize `WithFilter` for sorted collections
    *
    * @define coll sorted collection
    */
  class WithFilter[+A, +IterableCC[_], +CC[X] <: SortedSetOps[X, CC, _] with SortedSet[X]](
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
