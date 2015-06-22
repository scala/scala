/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package generic

/** Any collection (including maps) whose keys (or elements) are ordered.
 *
 *  @author Sean McDirmid
 *  @since  2.8
 */
trait Sorted[K, +This <: Sorted[K, This]] {
  def ordering : Ordering[K]

  /** The current collection */
  protected def repr: This

  /** return as a projection the set of keys in this collection */
  def keySet: SortedSet[K]

  /** Returns the first key of the collection. */
  def firstKey: K

  /** Returns the last key of the collection. */
  def lastKey: K

  /** Comparison function that orders keys. */
  def compare(k0: K, k1: K): Int = ordering.compare(k0, k1)

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
  def rangeImpl(from: Option[K], until: Option[K]): This

  /** Creates a ranged projection of this collection with no upper-bound.
   *
   *  @param from The lower-bound (inclusive) of the ranged projection.
   */
  def from(from: K): This = rangeImpl(Some(from), None)

  /** Creates a ranged projection of this collection with no lower-bound.
   *
   *  @param until The upper-bound (exclusive) of the ranged projection.
   */
  def until(until: K): This = rangeImpl(None, Some(until))

  /** Creates a ranged projection of this collection with both a lower-bound
   *  and an upper-bound.
   *
   *  @param from The lower-bound (inclusive) of the ranged projection.
   *  @param until The upper-bound (exclusive) of the ranged projection.
   */
  def range(from: K, until: K): This = rangeImpl(Some(from), Some(until))

  /** Create a range projection of this collection with no lower-bound.
   *  @param to The upper-bound (inclusive) of the ranged projection.
   */
  def to(to: K): This = {
    val i = keySet.from(to).iterator
    if (i.isEmpty) return repr
    val next = i.next()
    if (compare(next, to) == 0)
      if (i.isEmpty) repr
      else until(i.next())
    else
      until(next)
  }

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
  def keysIteratorFrom(start: K): Iterator[K]

  protected def hasAll(j: Iterator[K]): Boolean = {
    val i = keySet.iterator
    if (i.isEmpty) return j.isEmpty

    var in = i.next()
    while (j.hasNext) {
      val jn = j.next()
      while ({
        val n = compare(jn, in)
        if (n == 0) false
        else if (n < 0) return false
        else if (!i.hasNext) return false
        else true
      }) in = i.next()
    }
    true
  }
}
