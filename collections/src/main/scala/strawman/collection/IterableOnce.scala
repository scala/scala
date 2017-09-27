package strawman
package collection

import scala.{Any, Int}

trait IterableOnce[+A] extends Any {
  /** Iterator can be used only once */
  def iterator(): Iterator[A]

  def knownSize: Int
}
