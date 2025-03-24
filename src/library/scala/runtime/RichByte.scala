/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
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
  protected def num: scala.math.Numeric.ByteIsIntegral.type = scala.math.Numeric.ByteIsIntegral
  protected def ord: scala.math.Ordering.Byte.type = scala.math.Ordering.Byte

  override def doubleValue = self.toDouble
  override def floatValue  = self.toFloat
  override def longValue   = self.toLong
  override def intValue    = self.toInt
  override def byteValue   = self
  override def shortValue  = self.toShort

  override def isValidByte   = true

  // These method are all overridden and redefined to call out to scala.math to avoid 3 allocations:
  // the primitive boxing, the value class boxing and instantiation of the Numeric num.
  // We'd like to redefine signum and sign too but forwards binary compatibility doesn't allow us to.
  override def abs: Byte             = math.abs(self).toByte
  override def max(that: Byte): Byte = math.max(self, that).toByte
  override def min(that: Byte): Byte = math.min(self, that).toByte
}
