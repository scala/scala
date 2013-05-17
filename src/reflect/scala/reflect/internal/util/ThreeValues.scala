package scala
package reflect.internal.util

/** A simple three value type for booleans with an unknown value */
object ThreeValues {

  type ThreeValue = Byte

  final val YES = 1
  final val NO = -1
  final val UNKNOWN = 0

  def fromBoolean(b: Boolean): ThreeValue = if (b) YES else NO
  def toBoolean(x: ThreeValue): Boolean = x == YES
}
