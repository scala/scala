package strawman.collection

import scala.Ordering

/** Base trait for sorted collections */
trait Sorted[A] extends SortedLike[A, Sorted[A]]

trait SortedLike[A, +Repr] {

  def ordering: Ordering[A]

  def range(from: A, until: A): Repr
}
