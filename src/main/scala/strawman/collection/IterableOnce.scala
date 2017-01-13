package strawman
package collection

import strawman.collection.mutable.Iterator

trait IterableOnce[+A] {
  /** Iterator can be used only once */
  def iterator(): Iterator[A]
}