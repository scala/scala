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

  /**
    * @return `'''true'''` if this number has no decimal component.
    *         Always returns `'''true'''` for `RichInt`.
    */
  def isWhole() = true

  /**
    * @param end The final bound of the range to make.
    * @return A [[scala.collection.immutable.Range]] from `this` up to but
    *         not including `end`.
    */
  def until(end: Int): Range = Range(self, end)

  /**
    * @param end The final bound of the range to make.
    * @param step The number to increase by for each step of the range.
    * @return A [[scala.collection.immutable.Range]] from `this` up to but
    *         not including `end`.
    */
  def until(end: Int, step: Int): Range = Range(self, end, step)

//  @bridge
//  def until(end: Int): Range with Range.ByOne = new Range(self, end, 1) with Range.ByOne

  /** like `until`, but includes the last index */
  /**
    * @param end The final bound of the range to make.
    * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
    *         and including `end`.
    */
  def to(end: Int): Range.Inclusive = Range.inclusive(self, end)

  /**
    * @param end The final bound of the range to make.
    * @param step The number to increase by for each step of the range.
    * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
    *         and including `end`.
    */
  def to(end: Int, step: Int): Range.Inclusive = Range.inclusive(self, end, step)

//  @bridge
//  def to(end: Int): Range with Range.ByOne = new Range.Inclusive(self, end, 1) with Range.ByOne

  /**
    * @return `'''this'''` if `'''this''' < that` or `that` otherwise
    */
  override def min(that: Int): Int = if (self < that) self else that

  /**
    * @return `'''this'''` if `'''this''' > that` or `that` otherwise
    */
  override def max(that: Int): Int = if (self > that) self else that

  /**
    * Computes the absolute value of `'''this'''`.
    */
  override def abs: Int = if (self < 0) -self else self

  def toBinaryString: String = java.lang.Integer.toBinaryString(self)
  def toHexString: String = java.lang.Integer.toHexString(self)
  def toOctalString: String = java.lang.Integer.toOctalString(self)

  override def isValidByte = self.toByte.toInt == self
  override def isValidShort = self.toShort.toInt == self
  override def isValidChar = self.toChar.toInt == self
  override def isValidInt = true
  override def isValidLong = true
  override def isValidFloat = self.toFloat.toInt == self && self != Int.MaxValue
  override def isValidDouble = true
}
