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

final class RichLong(val self: Long) extends AnyVal with IntegralProxy[Long] {
  protected def num: scala.math.Numeric.LongIsIntegral.type = scala.math.Numeric.LongIsIntegral
  protected def ord: scala.math.Ordering.Long.type = scala.math.Ordering.Long

  override def doubleValue = self.toDouble
  override def floatValue  = self.toFloat
  override def longValue   = self
  override def intValue    = self.toInt
  override def byteValue   = self.toByte
  override def shortValue  = self.toShort

  override def isValidByte  = self.toByte.toLong == self
  override def isValidShort = self.toShort.toLong == self
  override def isValidChar  = self.toChar.toLong == self
  override def isValidInt   = self.toInt.toLong == self
           def isValidLong  = true
  // override def isValidFloat = self.toFloat.toLong == self && self != Long.MaxValue
  // override def isValidDouble = self.toDouble.toLong == self && self != Long.MaxValue

  // These method are all overridden and redefined to call out to scala.math to avoid 3 allocations:
  // the primitive boxing, the value class boxing and instantiation of the Numeric num.
  // We'd like to redefine signum and sign too but forwards binary compatibility doesn't allow us to.
  override def abs: Long             = math.abs(self)
  override def max(that: Long): Long = math.max(self, that)
  override def min(that: Long): Long = math.min(self, that)

  /** There is no reason to round a `Long`, but this method is provided to avoid accidental conversion to `Int` through `Float`. */
  @deprecated("this is an integer type; there is no reason to round it.  Perhaps you meant to call this on a floating-point value?", "2.11.0")
  def round: Long = self

  def toBinaryString: String = java.lang.Long.toBinaryString(self)
  def toHexString: String    = java.lang.Long.toHexString(self)
  def toOctalString: String  = java.lang.Long.toOctalString(self)
}
