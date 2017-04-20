package strawman.collection

import scala.{Ordering, Option, Some}

/** Base trait for sorted collections */
trait Sorted[A] extends SortedLike[A, Sorted[A]]

trait SortedLike[A, +Repr]
  extends SortedMonoTransforms[A, Repr] {

  def ordering: Ordering[A]

  /** Returns the first key of the collection. */
  def firstKey: A

  /** Returns the last key of the collection. */
  def lastKey: A

  /**
    * Creates an iterator over all the keys(or elements)  contained in this
    * collection greater than or equal to `start`
    * according to the ordering of this collection. x.keysIteratorFrom(y)
    * is equivalent to but often more efficient than
    * x.from(y).keysIterator.
    *
    * @param start The lower bound (inclusive)
    * on the keys to be returned
    */
  def keysIteratorFrom(start: A): Iterator[A]

}

trait SortedMonoTransforms[A, +Repr] {

  /** Creates a ranged projection of this collection. Any mutations in the
    *  ranged projection will update this collection and vice versa.
    *
    *  Note: keys are not guaranteed to be consistent between this collection
    *  and the projection. This is the case for buffers where indexing is
    *  relative to the projection.
    *
    *  @param from  The lower-bound (inclusive) of the ranged projection.
    *               `None` if there is no lower bound.
    *  @param until The upper-bound (exclusive) of the ranged projection.
    *               `None` if there is no upper bound.
    */
  def rangeImpl(from: Option[A], until: Option[A]): Repr

  /** Creates a ranged projection of this collection with both a lower-bound
    *  and an upper-bound.
    *
    *  @param from The lower-bound (inclusive) of the ranged projection.
    *  @param until The upper-bound (exclusive) of the ranged projection.
    */
  def range(from: A, until: A): Repr = rangeImpl(Some(from), Some(until))


}
