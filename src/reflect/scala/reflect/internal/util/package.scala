package scala.reflect
package internal

package object util {
  /**
   * Adds the `sm` String interpolator to a [[scala.StringContext]].
   */
  implicit class StringContextStripMarginOps(val stringContext: StringContext) extends StripMarginInterpolator
}
