/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: $

package scala

import java.math.{BigDecimal => BigDec, MathContext}

/**
 *  @author  Stephane Micheloud
 *  @version 1.0
 */
object BigDecimal {

  private val minCached = -256
  private val maxCached = 256
  private lazy val cache = new Array[BigDecimal](maxCached - minCached + 1)
  private lazy val cacheP = new Array[Array[BigDecimal]](maxCached - minCached + 1)

  object Precision extends Enumeration {
    type Precision = Value
    val DECIMAL128, DECIMAL32, DECIMAL64, UNLIMITED = Value
  }
  type Precision = Precision.Precision

  private val mc = collection.immutable.ListMap.empty[Precision, MathContext] +
    (Precision.DECIMAL128 -> MathContext.DECIMAL128) +
    (Precision.DECIMAL32  -> MathContext.DECIMAL32) +
    (Precision.DECIMAL64  -> MathContext.DECIMAL64) +
    (Precision.UNLIMITED  -> MathContext.UNLIMITED)

  /** Constructs a <code>BigDecimal</code> whose value is equal to that of the
   *  specified <code>Integer</code> value.
   *
   *  @param i the specified <code>Integer</code> value
   *  @param p the specified precision <code>p</code>
   *  @return  the constructed <code>BigDecimal</code>
   */
  def apply(i: Int, p: Precision): BigDecimal =
    if (minCached <= i && i <= maxCached) {
      val offset = i - minCached
      var a = cacheP(offset)
      if (a eq null) { a = new Array[BigDecimal](mc.size); cacheP(offset) = a }
      var n = a(p.id)
      if (n eq null) { n = new BigDecimal(BigDec.valueOf(i)); a(p.id) = n }
      n
    } else new BigDecimal(BigDec.valueOf(i))

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
   *  specified <code>Long</code> value.
   *
   *  @param l the specified long value
   *  @param p the specified precision
   *  @return  the constructed <code>BigDecimal</code>
   */
  def apply(l: Long, p: Precision): BigDecimal =
    if (minCached <= l && l <= maxCached) apply(l.toInt, p)
    else new BigDecimal(BigDec.valueOf(l))

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
   *  specified <code>Double</code> value.
   *
   *  @param d the specified <code>Double</code> value
   *  @param p the specified precision
   *  @return  the constructed <code>BigDecimal</code>
   */
  def apply(d: Double, p: Precision): BigDecimal =
    new BigDecimal(new BigDec(d, mc(p)))

  /** Constructs a <code>BigDecimal</code> whose value is equal to that of the
   *  specified double value.
   *
   *  @param d the specified <code>Double</code> value
   *  @return  the constructed <code>BigDecimal</code>
   */
  def apply(d: Double): BigDecimal =
    new BigDecimal(BigDec.valueOf(d))

  /** Translates a character array representation of a <code>BigDecimal</code>
   *  into a <code>BigDecimal</code>.
   */
  def apply(x: Array[Char]): BigDecimal =
    new BigDecimal(new BigDec(x))

  /** Translates the decimal String representation of a <code>BigDecimal</code>
   *  into a <code>BigDecimal</code>.
   */
  def apply(x: String, p: Precision): BigDecimal =
    new BigDecimal(new BigDec(x, mc(p)))

  /** Translates the decimal String representation of a <code>BigDecimal</code>
   *  into a <code>BigDecimal</code>.
   */
  def apply(x: String): BigDecimal =
    new BigDecimal(new BigDec(x))

  /** Constructs a <code>BigDecimal</code> whose value is equal to that of the
   *  specified <code>BigInt</code> value.
   *
   *  @param x the specified <code>BigInt</code> value
   *  @param p the specified precision <code>p</code>
   *  @return  the constructed <code>BigDecimal</code>
   */
  def apply(x: BigInt, p: Precision): BigDecimal =
    new BigDecimal(new BigDec(x.bigInteger, mc(p)))

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

  /** Implicit copnversion from <code>Long</code> to <code>BigDecimal</code>. */
  implicit def long2bigDecimal(l: Long): BigDecimal = apply(l)

  /** Implicit copnversion from <code>Double</code> to <code>BigDecimal</code>. */
  implicit def double2bigDecimal(d: Double): BigDecimal = apply(d)

  /** Implicit conversion from BigDecimal to <code>Ordered</code>. */
  implicit def bigDecimal2ordered(x: BigDecimal): Ordered[BigDecimal] =
    new Ordered[BigDecimal] with Proxy {
      def self: Any = x;
      def compare (y: BigDecimal): Int = x.bigDecimal.compareTo(y.bigDecimal)
    }
}

/**
 *  @author  Stephane Micheloud
 *  @version 1.0
 */
@serializable
class BigDecimal(val bigDecimal: BigDec) extends java.lang.Number {

  /** Returns the hash code for this BigDecimal. */
  override def hashCode(): Int = this.bigDecimal.hashCode()

  /** Compares this BigDecimal with the specified value for equality.
   */
  override def equals (that: Any): Boolean = that match {
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
  def +  (that: BigDecimal): BigDecimal = new BigDecimal(this.bigDecimal.add(that.bigDecimal))

  /** Subtraction of BigDecimals
   */
  def -  (that: BigDecimal): BigDecimal = new BigDecimal(this.bigDecimal.subtract(that.bigDecimal))

  /** Multiplication of BigDecimals
   */
  def *  (that: BigDecimal): BigDecimal = new BigDecimal(this.bigDecimal.multiply(that.bigDecimal))

  /** Division of BigDecimals
   */
  def /  (that: BigDecimal): BigDecimal = new BigDecimal(this.bigDecimal.divide(that.bigDecimal))

  /** Remainder of BigDecimals
   */
  def %  (that: BigDecimal): BigDecimal = new BigDecimal(this.bigDecimal.remainder(that.bigDecimal))

  /** Returns a pair of two BigDecimals containing (this / that) and (this % that).
   */
  def /% (that: BigDecimal): (BigDecimal, BigDecimal) = {
    val dr = this.bigDecimal.divideAndRemainder(that.bigDecimal)
    (new BigDecimal(dr(0)), new BigDecimal(dr(1)))
  }

  /** Returns the minimum of this and that
   */
  def min (that: BigDecimal): BigDecimal = new BigDecimal(this.bigDecimal.min(that.bigDecimal))

  /** Returns the maximum of this and that
   */
  def max (that: BigDecimal): BigDecimal = new BigDecimal(this.bigDecimal.max(that.bigDecimal))

  /** Returns a BigDecimal whose value is (<tt>this</tt> raised to the power of <tt>exp</tt>).
   */
  def pow (exp: Int): BigDecimal = new BigDecimal(this.bigDecimal.pow(exp))

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

  /** Returns the decimal String representation of this BigDecimal.
   */
  override def toString(): String = this.bigDecimal.toString()

  /** Returns a string representation of this BigDecimal  without an exponent field.
   */
  def toPlainString(): String = this.bigDecimal.toPlainString()

}
