package scala
package reflect
package internal

import Depth._

final class Depth private (val depth: Int) extends AnyVal with Ordered[Depth] {
  def max(that: Depth): Depth   = if (this < that) that else this
  def decr(n: Int): Depth       = if (isAnyDepth) this else Depth(depth - n)
  def incr(n: Int): Depth       = if (isAnyDepth) this else Depth(depth + n)
  def decr: Depth               = decr(1)
  def incr: Depth               = incr(1)

  def isNegative = depth < 0
  def isZero     = depth == 0
  def isAnyDepth = this == AnyDepth

  def compare(that: Depth): Int = if (depth < that.depth) -1 else if (this == that) 0 else 1
  override def toString = s"Depth($depth)"
}

object Depth {
  // A don't care value for the depth parameter in lubs/glbs and related operations.
  final val AnyDepth = new Depth(Int.MinValue)
  final val Zero     = new Depth(0)

  @inline final def apply(depth: Int): Depth = if (depth < 0) AnyDepth else new Depth(depth)
}
