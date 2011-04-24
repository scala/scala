/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.runtime

import scala.collection.immutable.Range
import annotation.bridge

// Note that this does not implement IntegralProxy[Int] so that it can return
// the Int-specific Range class from until/to.
final class RichInt(val self: Int) extends ScalaNumberProxy[Int] with RangedProxy[Int] {
  type ResultWithoutStep = Range
  def isWhole() = true

  def until(end: Int): Range = Range(self, end)
  def until(end: Int, step: Int): Range = Range(self, end, step)

  @bridge
  def until(end: Int): Range with Range.ByOne = new Range(self, end, 1) with Range.ByOne

  /** like `until`, but includes the last index */
  def to(end: Int): Range.Inclusive = Range.inclusive(self, end)
  def to(end: Int, step: Int): Range.Inclusive = Range.inclusive(self, end, step)

  @bridge
  def to(end: Int): Range with Range.ByOne = new Range.Inclusive(self, end, 1) with Range.ByOne

  override def min(that: Int): Int = if (self < that) self else that
  override def max(that: Int): Int = if (self > that) self else that
  override def abs: Int = if (self < 0) -self else self

  def toBinaryString: String = java.lang.Integer.toBinaryString(self)
  def toHexString: String = java.lang.Integer.toHexString(self)
  def toOctalString: String = java.lang.Integer.toOctalString(self)
}
