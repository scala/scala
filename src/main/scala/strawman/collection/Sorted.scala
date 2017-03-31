package strawman.collection

import scala.Ordering

/** Base trait for sorted collections */
trait Sorted[A] extends SortedLike[A, Sorted[A]]

trait SortedLike[A, +Repr] {

  def ordering: Ordering[A]

  def range(from: A, until: A): Repr

  /** Returns the first key of the collection. */
  def firstKey: A

  /** Returns the last key of the collection. */
  def lastKey: A

}
