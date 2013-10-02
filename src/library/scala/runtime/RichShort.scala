/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package runtime


final class RichShort(val self: Short) extends AnyVal with ScalaWholeNumberProxy[Short] {
  protected def num = scala.math.Numeric.ShortIsIntegral
  protected def ord = scala.math.Ordering.Short

  override def doubleValue() = self.toDouble
  override def floatValue()  = self.toFloat
  override def longValue()   = self.toLong
  override def intValue()    = self.toInt
  override def byteValue()   = self.toByte
  override def shortValue()  = self

  override def isValidShort  = true

  override def abs: Short              = math.abs(self.toInt).toShort
  override def max(that: Short): Short = math.max(self.toInt, that.toInt).toShort
  override def min(that: Short): Short = math.min(self.toInt, that.toInt).toShort
  override def signum: Int             = math.signum(self.toInt)
}
