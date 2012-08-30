/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

final class RichLong(val self: Long) extends AnyVal with IntegralProxy[Long] {
  protected def num = scala.math.Numeric.LongIsIntegral
  protected def ord = scala.math.Ordering.Long

  def toBinaryString: String = java.lang.Long.toBinaryString(self)
  def toHexString: String = java.lang.Long.toHexString(self)
  def toOctalString: String = java.lang.Long.toOctalString(self)

  override def isValidByte = self.toByte.toLong == self
  override def isValidShort = self.toShort.toLong == self
  override def isValidChar = self.toChar.toLong == self
  override def isValidInt = self.toInt.toLong == self
  // override def isValidLong = true
  // override def isValidFloat = self.toFloat.toLong == self && self != Long.MaxValue
  // override def isValidDouble = self.toDouble.toLong == self && self != Long.MaxValue
}
