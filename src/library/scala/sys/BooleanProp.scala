/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.sys

/** A few additional conveniences for Boolean properties.
 */
trait BooleanProp extends Prop[Boolean] {
  /** The default implementation of `value` adheres to java's definition
   *  of truth, which means it is true only if there is a value in the map and
   *  that value, once converted to all lower case, is equal to "true".
   *
   *  @return   true if the current String is considered true, false otherwise
   */
  def value: Boolean

  /** Alter this property so that `value` will be true. */
  def enable(): Unit

  /** Alter this property so that `value` will be false. */
  def disable(): Unit

  /** Toggle the property between enabled and disabled states. */
  def toggle(): Unit
}

object BooleanProp {
  implicit def booleanPropAsBoolean(b: BooleanProp): Boolean = b.value
}
