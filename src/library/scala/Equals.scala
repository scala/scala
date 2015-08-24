/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** An interface containing operations for equality.
 *  The only method not already present in class `AnyRef` is `canEqual`.
 */
trait Equals extends Any {
  /** A method that should be called from every well-designed equals method
   *  that is open to be overridden in a subclass. See
   *  [[http://www.artima.com/pins1ed/object-equality.html Programming in Scala,
   *  Chapter 28]] for discussion and design.
   *
   *  @param    that    the value being probed for possible equality
   *  @return   true if this instance can possibly equal `that`, otherwise false
   */
  def canEqual(that: Any): Boolean

  /** The universal equality method defined in `AnyRef`.
   */
  def equals(that: Any): Boolean
}
