package strawman
package collection

trait IterableOnce[+A] {
  /** Iterator can be used only once */
  def iterator(): Iterator[A]
}
