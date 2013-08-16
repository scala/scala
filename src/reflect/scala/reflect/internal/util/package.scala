package scala.reflect
package internal

import scala.collection.generic.GenericTraversableTemplate
import scala.collection.GenTraversableOnce

package object util {
  /** Adds the `sm` String interpolator to a [[scala.StringContext]].
   */
  implicit class StringContextStripMarginOps(val stringContext: StringContext) extends StripMarginInterpolator

  /** Adds the `sequence` method to a traversable `xss` that has traversables as elements.
   *  `xss.sequence` returns `None` if `xss` contains an empty traversable,
   *  or else `Some(xss.flatten)`.
   */
  implicit class Sequenceable[CC[X] <: Traversable[X] with GenericTraversableTemplate[X, CC], A](xss: CC[A]) {
    def sequence[B](implicit asTraversable: A => GenTraversableOnce[B]): Option[CC[B]] = {
      val b = xss.genericBuilder[B]
      for (xs <- xss)
        if (xs.isEmpty) return None
        else b ++= asTraversable(xs).seq
      Some(b.result)
    }
  }
}
