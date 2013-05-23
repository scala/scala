/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package runtime


final class RichByte(val self: Byte) extends AnyVal with ScalaWholeNumberProxy[Byte] {
  protected def num = scala.math.Numeric.ByteIsIntegral
  protected def ord = scala.math.Ordering.Byte

  override def doubleValue() = self.toDouble
  override def floatValue()  = self.toFloat
  override def longValue()   = self.toLong
  override def intValue()    = self.toInt
  override def byteValue()   = self
  override def shortValue()  = self.toShort

  override def isValidByte   = true

  override def abs: Byte             = math.abs(self).toByte
  override def max(that: Byte): Byte = math.max(self, that).toByte
  override def min(that: Byte): Byte = math.min(self, that).toByte
  override def signum: Int           = math.signum(self.toInt)
}
