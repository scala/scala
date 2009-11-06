/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime

import scala.collection.immutable.{Range, NumericRange}

final class RichLong(x: Long) extends Proxy with Ordered[Long] {

  // Proxy.self
  def self: Any = x

  // Ordered[Long].compare
  def compare(y: Long): Int = if (x < y) -1 else if (x > y) 1 else 0

  /** Create a NumericRange[Long] in range <code>[start;end)</code>
   *  with the specified step, where start is the target Long.
   *
   *  @param end    the end value of the range (exclusive)
   *  @param step   the distance between elements (defaults to 1)
   *  @return       the range
   */
  def until(end: Long, step: Long = 1L): NumericRange.Exclusive[Long] = Range.Long(x, end, step)

  /** Like until, but inclusive of the end value.
   */
  def to(end: Long, step: Long = 1L): NumericRange.Inclusive[Long] = Range.Long.inclusive(x, end, step)

  def min(y: Long): Long = if (x < y) x else y
  def max(y: Long): Long = if (x > y) x else y
  def abs: Long = if (x < 0) -x else x

  def toBinaryString: String = java.lang.Long.toBinaryString(x)
  def toHexString: String = java.lang.Long.toHexString(x)
  def toOctalString: String = java.lang.Long.toOctalString(x)
}
