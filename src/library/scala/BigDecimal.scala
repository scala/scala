/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala

import java.math.{BigDecimal => BigDec}

/**
 *  @author  Stephane Micheloud
 *  @version 1.0
 */
object BigDecimal {

  @serializable
  object RoundingMode extends Enumeration {
    type RoundingMode = Value
    val ROUND_UP, ROUND_DOWN, ROUND_CEILING, ROUND_FLOOR, ROUND_HALF_UP,
        ROUND_HALF_DOWN, ROUND_HALF_EVEN, ROUND_UNNECESSARY = Value
  }

  private val minCached = -512
  private val maxCached = 512
  private lazy val cache = new Array[BigDecimal](maxCached - minCached + 1)

  /** Constructs a <code>BigDecimal</code> whose value is equal to that of the
   *  specified <code>Integer</code> value.
   *
   *  @param i the specified integer value
   *  @return  the constructed <code>BigDecimal</code>
   */
  def apply(i: Int): BigDecimal =
    if (minCached <= i && i <= maxCached) {
      val offset = i - minCached
      var n = cache(offset)
      if (n eq null) { n = new BigDecimal(BigDec.valueOf(i)); cache(offset) = n }
      n
    } else new BigDecimal(BigDec.valueOf(i))

  /** Constructs a <code>BigDecimal</code> whose value is equal to that of the
   *  specified long value.
   *
   *  @param l the specified long value
   *  @return  the constructed <code>BigDecimal</code>
   */
  def apply(l: Long): BigDecimal =
    if (minCached <= l && l <= maxCached) apply(l.toInt)
    else new BigDecimal(BigDec.valueOf(l))

  /** Constructs a <code>BigDecimal</code> whose value is equal to that of the
   *  specified double value.
   *
   *  @param d the specified <code>Double</code> value
   *  @return  the constructed <code>BigDecimal</code>
   */
  def apply(d: Double): BigDecimal =
    new BigDecimal(new BigDec(d))

  /** Translates a character array representation of a <code>BigDecimal</code>
   *  into a <code>BigDecimal</code>.
   */
  def apply(x: Array[Char]): BigDecimal =
    new BigDecimal(new BigDec(x.toString))

  /** Translates the decimal String representation of a <code>BigDecimal</code>
   *  into a <code>BigDecimal</code>.
   */
  def apply(x: String): BigDecimal =
    new BigDecimal(new BigDec(x))

  /** Constructs a <code>BigDecimal</code> whose value is equal to that of the
   *  specified <code>BigInt</code> value.
   *
   *  @param x the specified <code>BigInt</code> value
   *  @return  the constructed <code>BigDecimal</code>
   */
  def apply(x: BigInt): BigDecimal =
    new BigDecimal(new BigDec(x.bigInteger))

  /** Implicit conversion from <code>Int</code> to <code>BigDecimal</code>. */
  implicit def int2bigDecimal(i: Int): BigDecimal = apply(i)

  /** Implicit conversion from <code>Long</code> to <code>BigDecimal</code>. */
  implicit def long2bigDecimal(l: Long): BigDecimal = apply(l)

  /** Implicit conversion from <code>Float</code> to <code>BigDecimal</code>.
   *  @since 2.8
   */
  implicit def float2bigDecimal(f: Float): BigDecimal = apply(f)

  /** Implicit conversion from <code>Double</code> to <code>BigDecimal</code>. */
  implicit def double2bigDecimal(d: Double): BigDecimal = apply(d)

  /** Implicit conversion from <code>String</code> to <code>BigDecimal</code>.
   *  @since 2.8
   */
  implicit def string2bigDecimal(s: String): BigDecimal = apply(s)

  /** Implicit conversion from <code>BigInt</code> to <code>BigDecimal</code>.
   *  @since 2.8
   */
  implicit def bigInt2bigDecimal(x: BigInt): BigDecimal = apply(x)
}

/**
 *  @author  Stephane Micheloud
 *  @version 1.0
 */
@serializable
class BigDecimal(val bigDecimal: BigDec) extends java.lang.Number {
  import BigDecimal.RoundingMode._
  // import BigDecimal.RoundingMode.{ RoundingMode, ROUND_UP, ROUND_DOWN, ROUND_CEILING, ROUND_FLOOR,
  //     ROUND_HALF_UP, ROUND_HALF_DOWN, ROUND_HALF_EVEN, ROUND_UNNECESSARY }

  /** Returns the hash code for this BigDecimal. */
  override def hashCode(): Int = this.bigDecimal.hashCode()

  /** Compares this BigDecimal with the specified value for equality.
   */
  override def equals(that: Any): Boolean = that match {
    case that: BigDecimal => this equals that
    case that: java.lang.Double => this.bigDecimal.doubleValue == that.doubleValue
    case that: java.lang.Float  => this.bigDecimal.floatValue == that.floatValue
    case that: java.lang.Number => this equals BigDecimal(that.longValue)
    case that: java.lang.Character => this equals BigDecimal(that.charValue.asInstanceOf[Int])
    case _ => false
  }

  /** Compares this BigDecimal with the specified BigDecimal for equality.
   */
  def equals (that: BigDecimal): Boolean =
    this.bigDecimal.compareTo(that.bigDecimal) == 0

  /** Compares this BigDecimal with the specified BigDecimal
   */
  def compare (that: BigDecimal): Int = this.bigDecimal.compareTo(that.bigDecimal)

  /** Less-than-or-equals comparison of BigDecimals
   */
  def <= (that: BigDecimal): Boolean = this.bigDecimal.compareTo(that.bigDecimal) <= 0

  /** Greater-than-or-equals comparison of BigDecimals
   */
  def >= (that: BigDecimal): Boolean = this.bigDecimal.compareTo(that.bigDecimal) >= 0

  /** Less-than of BigDecimals
   */
  def <  (that: BigDecimal): Boolean = this.bigDecimal.compareTo(that.bigDecimal) <  0

  /** Greater-than comparison of BigDecimals
   */
  def >  (that: BigDecimal): Boolean = this.bigDecimal.compareTo(that.bigDecimal) > 0

  /** Addition of BigDecimals
   */
  def +  (that: BigDecimal): BigDecimal =
    new BigDecimal(this.bigDecimal.add(that.bigDecimal))

  /** Subtraction of BigDecimals
   */
  def -  (that: BigDecimal): BigDecimal =
    new BigDecimal(this.bigDecimal.subtract(that.bigDecimal))

  /** Multiplication of BigDecimals
   */
  def *  (that: BigDecimal): BigDecimal =
    new BigDecimal(this.bigDecimal.multiply(that.bigDecimal))

  /** Division of BigDecimals
   */
  def /  (that: BigDecimal): BigDecimal =
    new BigDecimal(this.bigDecimal.divide(that.bigDecimal))

  /** Returns the minimum of this and that
   */
  def min (that: BigDecimal): BigDecimal =
    new BigDecimal(this.bigDecimal.min(that.bigDecimal))

  /** Returns the maximum of this and that
   */
  def max (that: BigDecimal): BigDecimal =
    new BigDecimal(this.bigDecimal.max(that.bigDecimal))

  /** Returns a BigDecimal whose value is the negation of this BigDecimal
   */
  def unary_- : BigDecimal = new BigDecimal(this.bigDecimal.negate())

  /** Returns the absolute value of this BigDecimal
   */
  def abs: BigDecimal = new BigDecimal(this.bigDecimal.abs())

  /** Returns the sign of this BigDecimal, i.e.
   *   -1 if it is less than 0,
   *   +1 if it is greater than 0
   *   0  if it is equal to 0
   */
  def signum: Int = this.bigDecimal.signum()

  /** Returns the scale of this <code>BigDecimal</code>.
   */
  def scale: Int = this.bigDecimal.scale()

  /** Returns a <code>BigDecimal</code> whose scale is the specified value, and whose value is
   *  numerically equal to this BigDecimal's.
   */
  def setScale(scale: Int): BigDecimal =
    new BigDecimal(this.bigDecimal setScale scale)

  def setScale(scale: Int, mode: RoundingMode): BigDecimal =
    new BigDecimal(this.bigDecimal.setScale(scale, mode.id))

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
   *  if this BigDecimal has too great a magnitude to represent as a float,
   *  it will be converted to <code>Float.NEGATIVE_INFINITY</code> or
   *  <code>Float.POSITIVE_INFINITY</code> as appropriate.
   */
  def doubleValue = this.bigDecimal.doubleValue

  /** Converts this <code>BigDecimal</code> to a BigInteger.
   */
  def toBigInt(): BigInt = new BigInt(this.bigDecimal.toBigInteger())

  /** Returns the decimal String representation of this BigDecimal.
   */
  override def toString(): String = this.bigDecimal.toString()

}
