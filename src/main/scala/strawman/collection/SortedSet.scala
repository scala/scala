package strawman.collection

import strawman.collection.mutable.Builder
import scala.{Ordering, `inline`}
import scala.annotation.unchecked.uncheckedVariance

/** Base type of sorted sets */
trait SortedSet[A] extends Set[A] with SortedSetOps[A, SortedSet, SortedSet[A]]

trait SortedSetOps[A, +CC[X] <: SortedSet[X] with SortedSetOps[X, CC, _], +C <: SortedSet[A]]
  extends SetOps[A, Set, C]
     with SortedOps[A, C] {

  protected[this] def orderedFromIterable[B: Ordering](it: Iterable[B]): CC[B]

  def firstKey: A = head
  def lastKey: A = last

  /** Map */
  def map[B : Ordering](f: A => B): CC[B] = orderedFromIterable(View.Map(coll, f))

  /** Flatmap */
  def flatMap[B : Ordering](f: A => IterableOnce[B]): CC[B] = orderedFromIterable(View.FlatMap(coll, f))

  /** Returns a new $coll containing the elements from the left hand operand followed by the elements from the
    *  right hand operand. The element type of the $coll is the most specific superclass encompassing
    *  the element types of the two operands.
    *
    *  @param xs   the traversable to append.
    *  @tparam B   the element type of the returned collection.
    *  @return     a new collection of type `CC[B]` which contains all elements
    *              of this $coll followed by all elements of `xs`.
    */
  def concat[B >: A : Ordering](xs: IterableOnce[B]): CC[B] = orderedFromIterable(View.Concat(coll, xs))

  /** Alias for `concat` */
  @`inline` final def ++ [B >: A : Ordering](xs: IterableOnce[B]): CC[B] = concat(xs)

  /** Zip. Interesting because it requires to align to source collections. */
  def zip[B](xs: IterableOnce[B])(implicit ev: Ordering[(A @uncheckedVariance, B)]): CC[(A @uncheckedVariance, B)] = orderedFromIterable(View.Zip(coll, xs))
  // sound bcs of VarianceNote

  def collect[B: Ordering](pf: scala.PartialFunction[A, B]): CC[B] = flatMap(a =>
    if (pf.isDefinedAt(a)) View.Elems(pf(a))
    else View.Empty
  )
}

object SortedSet extends OrderedSetFactory[SortedSet] {
  def empty[A : Ordering]: SortedSet[A] = immutable.SortedSet.empty
  def orderedFromIterable[E : Ordering](it: Iterable[E]): SortedSet[E] = immutable.SortedSet.orderedFromIterable(it)
}

