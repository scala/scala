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

final class RichShort(val self: Short) extends AnyVal with ScalaWholeNumberProxy[Short] {
  protected def num: scala.math.Numeric.ShortIsIntegral.type = scala.math.Numeric.ShortIsIntegral
  protected def ord: scala.math.Ordering.Short.type = scala.math.Ordering.Short

  override def doubleValue = self.toDouble
  override def floatValue  = self.toFloat
  override def longValue   = self.toLong
  override def intValue    = self.toInt
  override def byteValue   = self.toByte
  override def shortValue  = self

  override def isValidShort  = true

  // These method are all overridden and redefined to call out to scala.math to avoid 3 allocations:
  // the primitive boxing, the value class boxing and instantiation of the Numeric num.
  // We'd like to redefine signum and sign too but forwards binary compatibility doesn't allow us to.
  override def abs: Short              = math.abs(self.toInt).toShort
  override def max(that: Short): Short = math.max(self.toInt, that.toInt).toShort
  override def min(that: Short): Short = math.min(self.toInt, that.toInt).toShort
}
