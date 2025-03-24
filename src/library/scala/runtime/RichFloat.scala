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

final class RichFloat(val self: Float) extends AnyVal with FractionalProxy[Float] {
  protected def num: Fractional[Float] = scala.math.Numeric.FloatIsFractional
  protected def ord: Ordering[Float]   = scala.math.Ordering.Float.TotalOrdering

  override def doubleValue = self.toDouble
  override def floatValue  = self
  override def longValue   = self.toLong
  override def intValue    = self.toInt
  override def byteValue   = self.toByte
  override def shortValue  = self.toShort

  override def isWhole = {
    val l = self.toLong
    l.toFloat == self || l == Long.MaxValue && self < Float.PositiveInfinity || l == Long.MinValue && self > Float.NegativeInfinity
  }
  override def isValidByte  = self.toByte.toFloat == self
  override def isValidShort = self.toShort.toFloat == self
  override def isValidChar  = self.toChar.toFloat == self
  override def isValidInt   = { val i = self.toInt; i.toFloat == self && i != Int.MaxValue }
  // override def isValidLong = { val l = self.toLong; l.toFloat == self && l != Long.MaxValue }
  // override def isValidFloat = !java.lang.Float.isNaN(self)
  // override def isValidDouble = !java.lang.Float.isNaN(self)

  def isNaN: Boolean         = java.lang.Float.isNaN(self)
  def isInfinity: Boolean    = java.lang.Float.isInfinite(self)
  def isFinite: Boolean      = java.lang.Float.isFinite(self)
  def isPosInfinity: Boolean = Float.PositiveInfinity == self
  def isNegInfinity: Boolean = Float.NegativeInfinity == self

  // These method are all overridden and redefined to call out to scala.math to avoid 3 allocations:
  // the primitive boxing, the value class boxing and instantiation of the Numeric num.
  // We'd like to redefine sign too but forwards binary compatibility doesn't allow us to.
  override def abs: Float              = math.abs(self)
  override def max(that: Float): Float = math.max(self, that)
  override def min(that: Float): Float = math.min(self, that)
  @deprecated("signum does not handle -0.0f or Float.NaN; use `sign` method instead", since = "2.13.0")
  override def signum: Int             = math.signum(self).toInt

  def round: Int   = math.round(self)
  def ceil: Float  = math.ceil(self.toDouble).toFloat
  def floor: Float = math.floor(self.toDouble).toFloat

  /** Converts an angle measured in degrees to an approximately equivalent
   *  angle measured in radians.
   *
   *  @return the measurement of the angle `x` in radians.
   */
  def toRadians: Float = math.toRadians(self.toDouble).toFloat

  /** Converts an angle measured in radians to an approximately equivalent
   *  angle measured in degrees.
   *
   *  @return the measurement of the angle `x` in degrees.
   */
  def toDegrees: Float = math.toDegrees(self.toDouble).toFloat
}
