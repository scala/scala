/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.math

import java.{ lang => jl }
import java.math.{ MathContext, BigDecimal => BigDec }
import scala.collection.immutable.NumericRange

import annotation.migration

/**
 *  @author  Stephane Micheloud
 *  @version 1.0
 *  @since 2.7
 */
object BigDecimal {
  private val minCached = -512
  private val maxCached = 512

  val defaultMathContext = MathContext.UNLIMITED

  val MinLong = new BigDecimal(BigDec valueOf Long.MinValue, defaultMathContext)
  val MaxLong = new BigDecimal(BigDec valueOf Long.MaxValue, defaultMathContext)

  /** Cache ony for defaultMathContext using BigDecimals in a small range. */
  private lazy val cache = new Array[BigDecimal](maxCached - minCached + 1)

  @serializable
  object RoundingMode extends Enumeration(java.math.RoundingMode.values map (_.toString) : _*) {
    type RoundingMode = Value
    val UP, DOWN, CEILING, FLOOR, HALF_UP, HALF_DOWN, HALF_EVEN, UNNECESSARY = Value
  }

  /** Constructs a <code>BigDecimal</code> using the java BigDecimal static
   *  valueOf constructor.
   *
   *  @param  d the specified double value
   *  @return the constructed <code>BigDecimal</code>
   */
  def valueOf(d: Double): BigDecimal = apply(BigDec valueOf d)
  def valueOf(d: Double, mc: MathContext): BigDecimal = apply(BigDec valueOf d, mc)

  /** Constructs a <code>BigDecimal</code> whose value is equal to that of the
   *  specified <code>Integer</code> value.
   *
   *  @param i the specified integer value
   *  @return  the constructed <code>BigDecimal</code>
   */
  def apply(i: Int): BigDecimal = apply(i, defaultMathContext)
  def apply(i: Int, mc: MathContext): BigDecimal =
    if (mc == defaultMathContext && minCached <= i && i <= maxCached) {
      val offset = i - minCached
      var n = cache(offset)
      if (n eq null) { n = new BigDecimal(BigDec.valueOf(i), mc); cache(offset) = n }
      n
    }
    else new BigDecimal(BigDec.valueOf(i), mc)

  /** Constructs a <code>BigDecimal</code> whose value is equal to that of the
   *  specified long value.
   *
   *  @param l the specified long value
   *  @return  the constructed <code>BigDecimal</code>
   */
  def apply(l: Long): BigDecimal =
    if (minCached <= l && l <= maxCached) apply(l.toInt)
    else new BigDecimal(BigDec.valueOf(l), defaultMathContext)

  def apply(l: Long, mc: MathContext): BigDecimal =
    new BigDecimal(new BigDec(l, mc), mc)

  /** Constructs a <code>BigDecimal</code> whose unscaled value is equal to that
   *  of the specified long value.
   *
   *  @param  unscaledVal the value
   *  @param  scale       the scale
   *  @return the constructed <code>BigDecimal</code>
   */
  def apply(unscaledVal: Long, scale: Int): BigDecimal =
    apply(BigInt(unscaledVal), scale)

  def apply(unscaledVal: Long, scale: Int, mc: MathContext): BigDecimal =
    apply(BigInt(unscaledVal), scale, mc)

  /** Constructs a <code>BigDecimal</code> whose value is equal to that of the
   *  specified double value.
   *
   *  @param d the specified <code>Double</code> value
   *  @return  the constructed <code>BigDecimal</code>
   */
  def apply(d: Double): BigDecimal = apply(d, defaultMathContext)
  // note we don't use the static valueOf because it doesn't let us supply
  // a MathContext, but we should be duplicating its logic, modulo caching.
  def apply(d: Double, mc: MathContext): BigDecimal =
    new BigDecimal(new BigDec(jl.Double.toString(d), mc), mc)

  /** Translates a character array representation of a <code>BigDecimal</code>
   *  into a <code>BigDecimal</code>.
   */
  def apply(x: Array[Char]): BigDecimal = apply(x, defaultMathContext)
  def apply(x: Array[Char], mc: MathContext): BigDecimal =
    new BigDecimal(new BigDec(x.mkString, mc), mc)

  /** Translates the decimal String representation of a <code>BigDecimal</code>
   *  into a <code>BigDecimal</code>.
   */
  def apply(x: String): BigDecimal = apply(x, defaultMathContext)
  def apply(x: String, mc: MathContext): BigDecimal =
    new BigDecimal(new BigDec(x, mc), mc)

  /** Constructs a <code>BigDecimal</code> whose value is equal to that of the
   *  specified <code>BigInt</code> value.
   *
   *  @param x the specified <code>BigInt</code> value
   *  @return  the constructed <code>BigDecimal</code>
   */
  def apply(x: BigInt): BigDecimal = apply(x, defaultMathContext)
  def apply(x: BigInt, mc: MathContext): BigDecimal =
    new BigDecimal(new BigDec(x.bigInteger, mc), mc)

  /** Constructs a <code>BigDecimal</code> whose unscaled value is equal to that
   *  of the specified <code>BigInt</code> value.
   *
   *  @param unscaledVal the specified <code>BigInt</code> value
   *  @param scale       the scale
   *  @return  the constructed <code>BigDecimal</code>
   */
  def apply(unscaledVal: BigInt, scale: Int): BigDecimal = apply(unscaledVal, scale, defaultMathContext)
  def apply(unscaledVal: BigInt, scale: Int, mc: MathContext): BigDecimal =
    new BigDecimal(new BigDec(unscaledVal.bigInteger, scale, mc), mc)

  def apply(bd: BigDec): BigDecimal = apply(bd, defaultMathContext)
  def apply(bd: BigDec, mc: MathContext): BigDecimal = new BigDecimal(bd, mc)

  /** Implicit conversion from <code>Int</code> to <code>BigDecimal</code>. */
  implicit def int2bigDecimal(i: Int): BigDecimal = apply(i)

  /** Implicit conversion from <code>Long</code> to <code>BigDecimal</code>. */
  implicit def long2bigDecimal(l: Long): BigDecimal = apply(l)

  /** Implicit conversion from <code>Double</code> to <code>BigDecimal</code>. */
  implicit def double2bigDecimal(d: Double): BigDecimal = valueOf(d, defaultMathContext)
}

/**
 *  @author  Stephane Micheloud
 *  @version 1.0
 */
@serializable
class BigDecimal(
  val bigDecimal: BigDec,
  val mc: MathContext)
extends ScalaNumber with ScalaNumericConversions
{
  def this(bigDecimal: BigDec) = this(bigDecimal, BigDecimal.defaultMathContext)
  import BigDecimal.RoundingMode._

  /** Cuts way down on the wrapper noise. */
  private implicit def bigdec2BigDecimal(x: BigDec): BigDecimal = new BigDecimal(x, mc)

  /** Returns the hash code for this BigDecimal.
   *  Note that this does not use the underlying java object's
   *  hashCode because we compare BigDecimals with compareTo
   *  which deems 2 == 2.00, whereas in java these are unequal
   *  with unequal hashCodes.
   */
  override def hashCode(): Int =
    if (isWhole) unifiedPrimitiveHashcode
    else doubleValue.hashCode()

  /** Compares this BigDecimal with the specified value for equality.
   */
  override def equals (that: Any): Boolean = that match {
    case that: BigDecimal     => this equals that
    case that: BigInt         => this.toBigIntExact exists (that equals _)
    case _: Float | _: Double => unifiedPrimitiveEquals(that)
    case x                    => isWhole && this <= BigDecimal.MaxLong && this >= BigDecimal.MinLong && unifiedPrimitiveEquals(x)
  }

  protected[math] def isWhole = (this remainder 1) == BigDecimal(0)
  def underlying = bigDecimal

  /** Compares this BigDecimal with the specified BigDecimal for equality.
   */
  def equals (that: BigDecimal): Boolean = compare(that) == 0

  /** Compares this BigDecimal with the specified BigDecimal
   */
  def compare (that: BigDecimal): Int = this.bigDecimal compareTo that.bigDecimal

  /** Less-than-or-equals comparison of BigDecimals
   */
  def <= (that: BigDecimal): Boolean = compare(that) <= 0

  /** Greater-than-or-equals comparison of BigDecimals
   */
  def >= (that: BigDecimal): Boolean = compare(that) >= 0

  /** Less-than of BigDecimals
   */
  def <  (that: BigDecimal): Boolean = compare(that) <  0

  /** Greater-than comparison of BigDecimals
   */
  def >  (that: BigDecimal): Boolean = compare(that) > 0

  /** Addition of BigDecimals
   */
  def +  (that: BigDecimal): BigDecimal = this.bigDecimal.add(that.bigDecimal, mc)

  /** Subtraction of BigDecimals
   */
  def -  (that: BigDecimal): BigDecimal = this.bigDecimal.subtract(that.bigDecimal, mc)

  /** Multiplication of BigDecimals
   */
  def *  (that: BigDecimal): BigDecimal = this.bigDecimal.multiply(that.bigDecimal, mc)

  /** Division of BigDecimals
   */
  def /  (that: BigDecimal): BigDecimal = this.bigDecimal.divide(that.bigDecimal, mc)

  /** Division and Remainder - returns tuple containing the result of
   *  divideToIntegralValue and the remainder.
   */
  def /% (that: BigDecimal): (BigDecimal, BigDecimal) =
    this.bigDecimal.divideAndRemainder(that.bigDecimal, mc) match {
      case Array(q, r)  => (q, r)
    }

  /** Divide to Integral value.
   */
  def quot (that: BigDecimal): BigDecimal =
    this.bigDecimal.divideToIntegralValue(that.bigDecimal, mc)

  /** Returns the minimum of this and that
   */
  def min (that: BigDecimal): BigDecimal = this.bigDecimal min that.bigDecimal

  /** Returns the maximum of this and that
   */
  def max (that: BigDecimal): BigDecimal = this.bigDecimal max that.bigDecimal

  /** Remainder after dividing this by that.
   */
  def remainder (that: BigDecimal): BigDecimal = this.bigDecimal.remainder(that.bigDecimal, mc)

  /** Remainder after dividing this by that.
   */
  def % (that: BigDecimal): BigDecimal = this.remainder(that)

  /** Returns a BigDecimal whose value is this ** n.
   */
  def pow (n: Int): BigDecimal = this.bigDecimal.pow(n, mc)

  /** Returns a BigDecimal whose value is the negation of this BigDecimal
   */
  def unary_- : BigDecimal = this.bigDecimal.negate(mc)

  /** Returns the absolute value of this BigDecimal
   */
  def abs: BigDecimal = this.bigDecimal abs mc

  /** Returns the sign of this BigDecimal, i.e.
   *   -1 if it is less than 0,
   *   +1 if it is greater than 0
   *   0  if it is equal to 0
   */
  def signum: Int = this.bigDecimal.signum()

  /** Returns the precision of this <code>BigDecimal</code>.
   */
  def precision: Int = this.bigDecimal.precision()

  /** Returns a BigDecimal rounded according to the MathContext settings.
   */
  def round(mc: MathContext): BigDecimal = this.bigDecimal round mc

  /** Returns the scale of this <code>BigDecimal</code>.
   */
  def scale: Int = this.bigDecimal.scale()

  /** Returns the size of an ulp, a unit in the last place, of this BigDecimal.
   */
  def ulp: BigDecimal = this.bigDecimal.ulp

  /** Returns a new BigDecimal based on the supplied MathContext.
   */
  def apply(mc: MathContext): BigDecimal = BigDecimal(this.bigDecimal.toString, mc)

  /** Returns a <code>BigDecimal</code> whose scale is the specified value, and whose value is
   *  numerically equal to this BigDecimal's.
   */
  def setScale(scale: Int): BigDecimal = this.bigDecimal setScale scale

  def setScale(scale: Int, mode: RoundingMode): BigDecimal =
    this.bigDecimal.setScale(scale, mode.id)

  /** Converts this BigDecimal to a <tt>byte</tt>.
   *  If the BigDecimal is too big to fit in a byte, only the low-order 8 bits are returned.
   *  Note that this conversion can lose information about the overall magnitude of the
   *  BigDecimal value as well as return a result with the opposite sign.
   */
  override def byteValue   = intValue.toByte

  /** Converts this BigDecimal to a <tt>short</tt>.
   *  If the BigDecimal is too big to fit in a byte, only the low-order 16 bits are returned.
   *  Note that this conversion can lose information about the overall magnitude of the
   *  BigDecimal value as well as return a result with the opposite sign.
   */
  override def shortValue  = intValue.toShort

  /** Converts this BigDecimal to a <tt>char</tt>.
   *  If the BigDecimal is too big to fit in a char, only the low-order 16 bits are returned.
   *  Note that this conversion can lose information about the overall magnitude of the
   *  BigDecimal value and that it always returns a positive result.
   */
  def charValue   = intValue.toChar

  /** Converts this BigDecimal to an <tt>int</tt>.
   *  If the BigDecimal is too big to fit in a char, only the low-order 32 bits
   *  are returned. Note that this conversion can lose information about the
   *  overall magnitude of the BigDecimal value as well as return a result with
   *  the opposite sign.
   */
  def intValue    = this.bigDecimal.intValue

  /** Converts this BigDecimal to a <tt>Long</tt>.
   *  If the BigDecimal is too big to fit in a char, only the low-order 64 bits
   *  are returned. Note that this conversion can lose information about the
   *  overall magnitude of the BigDecimal value as well as return a result with
   *  the opposite sign.
   */
  def longValue   = this.bigDecimal.longValue

  /** Converts this BigDecimal to a <tt>float</tt>.
   *  if this BigDecimal has too great a magnitude to represent as a float,
   *  it will be converted to <code>Float.NEGATIVE_INFINITY</code> or
   *  <code>Float.POSITIVE_INFINITY</code> as appropriate.
   */
  def floatValue  = this.bigDecimal.floatValue

  /** Converts this BigDecimal to a <tt>Double</tt>.
   *  if this BigDecimal has too great a magnitude to represent as a double,
   *  it will be converted to <code>Double.NEGATIVE_INFINITY</code> or
   *  <code>Double.POSITIVE_INFINITY</code> as appropriate.
   */
  def doubleValue = this.bigDecimal.doubleValue

  /** This BigDecimal as an exact value.
   */
  def toByteExact = bigDecimal.byteValueExact
  def toShortExact = bigDecimal.shortValueExact
  def toIntExact = bigDecimal.intValueExact
  def toLongExact = bigDecimal.longValueExact

  /** Creates a partially constructed NumericRange[BigDecimal] in range
   *  <code>[start;end)</code>, where start is the target BigDecimal.  The step
   *  must be supplied via the "by" method of the returned object in order
   *  to receive the fully constructed range.  For example:
   * <pre>
   * val partial = BigDecimal(1.0) to 2.0       // not usable yet
   * val range = partial by 0.01                // now a NumericRange
   * val range2 = BigDecimal(0) to 1.0 by 0.01  // all at once of course is fine too
   * </pre>
   *
   *  @param end    the end value of the range (exclusive)
   *  @return       the partially constructed NumericRange
   */
  def until(end: BigDecimal): Range.Partial[BigDecimal, NumericRange.Exclusive[BigDecimal]] =
    new Range.Partial(until(end, _))

  /** Same as the one-argument <code>until</code>, but creates the range immediately. */
  def until(end: BigDecimal, step: BigDecimal) = Range.BigDecimal(this, end, step)

  /** Like <code>until</code>, but inclusive of the end value. */
  def to(end: BigDecimal): Range.Partial[BigDecimal, NumericRange.Inclusive[BigDecimal]] =
    new Range.Partial(to(end, _))

  /** Like <code>until</code>, but inclusive of the end value. */
  def to(end: BigDecimal, step: BigDecimal) = Range.BigDecimal.inclusive(this, end, step)

  /** Converts this <code>BigDecimal</code> to a scala.BigInt.
   */
  def toBigInt(): BigInt = new BigInt(this.bigDecimal.toBigInteger())

  /** Converts this <code>BigDecimal</code> to a scala.BigInt if it
   *  can be done losslessly, returning Some(BigInt) or None.
   */
  def toBigIntExact(): Option[BigInt] =
    try Some(new BigInt(this.bigDecimal.toBigIntegerExact()))
    catch { case _: ArithmeticException => None }

  /** Returns the decimal String representation of this BigDecimal.
   */
  override def toString(): String = this.bigDecimal.toString()

}
