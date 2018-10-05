/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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
