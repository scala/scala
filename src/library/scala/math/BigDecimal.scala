/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package math

import java.{ lang => jl }
import java.math.{ MathContext, BigDecimal => BigDec }
import scala.collection.immutable.NumericRange
import scala.language.implicitConversions


/** 
 *  @author  Stephane Micheloud
 *  @author  Rex Kerr
 *  @version 1.1
 *  @since 2.7
 */
object BigDecimal {
  private final val maximumHashScale = 4934           // Quit maintaining hash identity with BigInt beyond this scale
  private final val hashCodeNotComputed = 0x5D50690F  // Magic value (happens to be "BigDecimal" old MurmurHash3 value)
  private final val deci2binary = 3.3219280948873626  // Ratio of log(10) to log(2)
  private val minCached = -512
  private val maxCached = 512
  val defaultMathContext = MathContext.DECIMAL128

  /** Cache only for defaultMathContext using BigDecimals in a small range. */
  private lazy val cache = new Array[BigDecimal](maxCached - minCached + 1)

  object RoundingMode extends Enumeration {
    // Annoying boilerplate to ensure consistency with java.math.RoundingMode
    import java.math.{RoundingMode => RM}
    type RoundingMode = Value
    val UP          = Value(RM.UP.ordinal)
    val DOWN        = Value(RM.DOWN.ordinal)
    val CEILING     = Value(RM.CEILING.ordinal)
    val FLOOR       = Value(RM.FLOOR.ordinal)
    val HALF_UP     = Value(RM.HALF_UP.ordinal)
    val HALF_DOWN   = Value(RM.HALF_DOWN.ordinal)
    val HALF_EVEN   = Value(RM.HALF_EVEN.ordinal)
    val UNNECESSARY = Value(RM.UNNECESSARY.ordinal) 
  }
  
  /** Constructs a `BigDecimal` using the decimal text representation of `Double` value `d`, rounding if necessary. */
  def decimal(d: Double, mc: MathContext): BigDecimal =
    new BigDecimal(new BigDec(java.lang.Double.toString(d), mc), mc)

  /** Constructs a `BigDecimal` using the decimal text representation of `Double` value `d`. */
  def decimal(d: Double): BigDecimal = decimal(d, defaultMathContext)
  
  /** Constructs a `BigDecimal` using the decimal text representation of `Float` value `f`, rounding if necessary. 
   *  Note that `BigDecimal.decimal(0.1f) != 0.1f` since equality agrees with the `Double` representation, and
   *  `0.1 != 0.1f`.
   */
  def decimal(f: Float, mc: MathContext): BigDecimal =
    new BigDecimal(new BigDec(java.lang.Float.toString(f), mc), mc)

  /** Constructs a `BigDecimal` using the decimal text representation of `Float` value `f`.
   *  Note that `BigDecimal.decimal(0.1f) != 0.1f` since equality agrees with the `Double` representation, and
   *  `0.1 != 0.1f`.
   */
  def decimal(f: Float): BigDecimal = decimal(f, defaultMathContext)
  
  // This exists solely to avoid conversion from Int/Long to Float, screwing everything up.
  /** Constructs a `BigDecimal` from a `Long`, rounding if necessary.  This is identical to `BigDecimal(l, mc)`. */
  def decimal(l: Long, mc: MathContext): BigDecimal = apply(l, mc)
  
  // This exists solely to avoid conversion from Int/Long to Float, screwing everything up.
  /** Constructs a `BigDecimal` from a `Long`.  This is identical to `BigDecimal(l)`. */
  def decimal(l: Long): BigDecimal = apply(l)
  
  /** Constructs a `BigDecimal` using a `java.math.BigDecimal`, rounding if necessary. */
  def decimal(bd: BigDec, mc: MathContext): BigDecimal = new BigDecimal(bd.round(mc), mc)
  
  /** Constructs a `BigDecimal` by expanding the binary fraction
   *  contained by `Double` value `d` into a decimal representation,
   *  rounding if necessary.  When a `Float` is converted to a
   *  `Double`, the binary fraction is preserved, so this method
   *  also works for converted `Float`s.
   */
  def binary(d: Double, mc: MathContext): BigDecimal = new BigDecimal(new BigDec(d, mc), mc)
  
  /** Constructs a `BigDecimal` by expanding the binary fraction
   *  contained by `Double` value `d` into a decimal representation.
   *  Note: this also works correctly on converted `Float`s.
   */
  def binary(d: Double): BigDecimal = binary(d, defaultMathContext)
  
  /** Constructs a `BigDecimal` from a `java.math.BigDecimal`.  The
   *  precision is the default for `BigDecimal` or enough to represent
   *  the `java.math.BigDecimal` exactly, whichever is greater.
   */
  def exact(repr: BigDec): BigDecimal = {
    val mc = 
      if (repr.precision <= defaultMathContext.getPrecision) defaultMathContext
      else new MathContext(repr.precision, java.math.RoundingMode.HALF_EVEN)
    new BigDecimal(repr, mc)
  }
  
  /** Constructs a `BigDecimal` by fully expanding the binary fraction
   *  contained by `Double` value `d`, adjusting the precision as
   *  necessary.  Note: this works correctly on converted `Float`s also.
   */
  def exact(d: Double): BigDecimal = exact(new BigDec(d))
  
  /** Constructs a `BigDecimal` that exactly represents a `BigInt`.
   */
  def exact(bi: BigInt): BigDecimal = exact(new BigDec(bi.bigInteger))
  
  /** Constructs a `BigDecimal` that exactly represents a `Long`.  Note that
   *  all creation methods for `BigDecimal` that do not take a `MathContext`
   *  represent a `Long`; this is equivalent to `apply`, `valueOf`, etc..
   */
  def exact(l: Long): BigDecimal = apply(l)
  
  /** Constructs a `BigDecimal` that exactly represents the number
   *  specified in a `String`.
   */
  def exact(s: String): BigDecimal = exact(new BigDec(s))
  
  /** Constructs a `BigDecimal` that exactly represents the number
   *  specified in base 10 in a character array.
   */
 def exact(cs: Array[Char]): BigDecimal = exact(new BigDec(cs))
  

  /** Constructs a `BigDecimal` using the java BigDecimal static
   *  valueOf constructor.  Equivalent to `BigDecimal.decimal`.
   *
   *  @param  d the specified double value
   *  @return the constructed `BigDecimal`
   */
  def valueOf(d: Double): BigDecimal = apply(BigDec valueOf d)
  
  /** Constructs a `BigDecimal` using the java BigDecimal static
   *  valueOf constructor, specifying a `MathContext` that is
   *  used for computations but isn't used for rounding.  Use
   *  `BigDecimal.decimal` to use `MathContext` for rounding,
   *  or `BigDecimal(java.math.BigDecimal.valueOf(d), mc)` for
   *  no rounding.
   *
   *  @param  d the specified double value
   *  @param  mc the `MathContext` used for future computations
   *  @return the constructed `BigDecimal`
   */
  @deprecated("MathContext is not applied to Doubles in valueOf.  Use BigDecimal.decimal to use rounding, or java.math.BigDecimal.valueOf to avoid it.","2.11")
  def valueOf(d: Double, mc: MathContext): BigDecimal = apply(BigDec valueOf d, mc)
  
  /** Constructs a `BigDecimal` using the java BigDecimal static
   *  valueOf constructor.
   *
   *  @param  x the specified `Long` value
   *  @return the constructed `BigDecimal`
   */
  def valueOf(x: Long): BigDecimal = apply(x)
  
  /** Constructs a `BigDecimal` using the java BigDecimal static
   *  valueOf constructor.  This is unlikely to do what you want;
   *  use `valueOf(f.toDouble)` or `decimal(f)` instead.
   */
  @deprecated("Float arguments to valueOf may not do what you wish.  Use decimal or valueOf(f.toDouble).","2.11")
  def valueOf(f: Float): BigDecimal = valueOf(f.toDouble)
  
  /** Constructs a `BigDecimal` using the java BigDecimal static
   *  valueOf constructor.  This is unlikely to do what you want;
   *  use `valueOf(f.toDouble)` or `decimal(f)` instead.
   */
  @deprecated("Float arguments to valueOf may not do what you wish.  Use decimal or valueOf(f.toDouble).","2.11")
  def valueOf(f: Float, mc: MathContext): BigDecimal = valueOf(f.toDouble, mc)

  
  /** Constructs a `BigDecimal` whose value is equal to that of the
   *  specified `Integer` value.
   *
   *  @param i the specified integer value
   *  @return  the constructed `BigDecimal`
   */
  def apply(i: Int): BigDecimal = apply(i, defaultMathContext)

  /** Constructs a `BigDecimal` whose value is equal to that of the
   *  specified `Integer` value, rounding if necessary.
   *
   *  @param i the specified integer value
   *  @param mc the precision and rounding mode for creation of this value and future operations on it
   *  @return  the constructed `BigDecimal`
   */
  def apply(i: Int, mc: MathContext): BigDecimal =
    if (mc == defaultMathContext && minCached <= i && i <= maxCached) {
      val offset = i - minCached
      var n = cache(offset)
      if (n eq null) { n = new BigDecimal(BigDec.valueOf(i.toLong), mc); cache(offset) = n }
      n
    }
    else apply(i.toLong, mc)

  /** Constructs a `BigDecimal` whose value is equal to that of the
   *  specified long value.
   *
   *  @param l the specified long value
   *  @return  the constructed `BigDecimal`
   */
  def apply(l: Long): BigDecimal =
    if (minCached <= l && l <= maxCached) apply(l.toInt)
    else new BigDecimal(BigDec.valueOf(l), defaultMathContext)

  /** Constructs a `BigDecimal` whose value is equal to that of the
   *  specified long value, but rounded if necessary.
   *
   *  @param l the specified long value
   *  @param mc the precision and rounding mode for creation of this value and future operations on it
   *  @return  the constructed `BigDecimal`
   */
  def apply(l: Long, mc: MathContext): BigDecimal =
    new BigDecimal(new BigDec(l, mc), mc)

  /** Constructs a `BigDecimal` whose unscaled value is equal to that
   *  of the specified long value.
   *
   *  @param  unscaledVal the value
   *  @param  scale       the scale
   *  @return the constructed `BigDecimal`
   */
  def apply(unscaledVal: Long, scale: Int): BigDecimal =
    apply(BigInt(unscaledVal), scale)

  /** Constructs a `BigDecimal` whose unscaled value is equal to that
   *  of the specified long value, but rounded if necessary.
   *
   *  @param  unscaledVal the value
   *  @param  scale       the scale
   *  @param mc the precision and rounding mode for creation of this value and future operations on it
   *  @return the constructed `BigDecimal`
   */
  def apply(unscaledVal: Long, scale: Int, mc: MathContext): BigDecimal =
    apply(BigInt(unscaledVal), scale, mc)

  /** Constructs a `BigDecimal` whose value is equal to that of the
   *  specified double value.  Equivalent to `BigDecimal.decimal`.
   *
   *  @param d the specified `Double` value
   *  @return  the constructed `BigDecimal`
   */
  def apply(d: Double): BigDecimal = decimal(d, defaultMathContext)
  
  // note we don't use the static valueOf because it doesn't let us supply
  // a MathContext, but we should be duplicating its logic, modulo caching.
  /** Constructs a `BigDecimal` whose value is equal to that of the
   *  specified double value, but rounded if necessary.  Equivalent to
   *  `BigDecimal.decimal`.
   *
   *  @param d the specified `Double` value
   *  @param mc the precision and rounding mode for creation of this value and future operations on it
   *  @return  the constructed `BigDecimal`
   */
  def apply(d: Double, mc: MathContext): BigDecimal = decimal(d, mc)

  @deprecated("The default conversion from Float may not do what you want.  Use BigDecimal.decimal for a String representation, or explicitly convert the Float with .toDouble.", "2.11")
  def apply(x: Float): BigDecimal = apply(x.toDouble)

  @deprecated("The default conversion from Float may not do what you want.  Use BigDecimal.decimal for a String representation, or explicitly convert the Float with .toDouble.", "2.11")
  def apply(x: Float, mc: MathContext): BigDecimal = apply(x.toDouble, mc)

  /** Translates a character array representation of a `BigDecimal`
   *  into a `BigDecimal`.
   */
  def apply(x: Array[Char]): BigDecimal = exact(x)

  /** Translates a character array representation of a `BigDecimal`
   *  into a `BigDecimal`, rounding if necessary.
   */
  def apply(x: Array[Char], mc: MathContext): BigDecimal =
    new BigDecimal(new BigDec(x, mc), mc)

  /** Translates the decimal String representation of a `BigDecimal`
   *  into a `BigDecimal`.
   */
  def apply(x: String): BigDecimal = exact(x)
  
  /** Translates the decimal String representation of a `BigDecimal`
   *  into a `BigDecimal`, rounding if necessary.
   */
  def apply(x: String, mc: MathContext): BigDecimal =
    new BigDecimal(new BigDec(x, mc), mc)

  /** Constructs a `BigDecimal` whose value is equal to that of the
   *  specified `BigInt` value.
   *
   *  @param x the specified `BigInt` value
   *  @return  the constructed `BigDecimal`
   */
  def apply(x: BigInt): BigDecimal = exact(x)
  
  /** Constructs a `BigDecimal` whose value is equal to that of the
   *  specified `BigInt` value, rounding if necessary.
   *
   *  @param x  the specified `BigInt` value
   *  @param mc the precision and rounding mode for creation of this value and future operations on it   
   *  @return   the constructed `BigDecimal`
   */
  def apply(x: BigInt, mc: MathContext): BigDecimal =
    new BigDecimal(new BigDec(x.bigInteger, mc), mc)

  /** Constructs a `BigDecimal` whose unscaled value is equal to that
   *  of the specified `BigInt` value.
   *
   *  @param unscaledVal the specified `BigInt` value
   *  @param scale       the scale
   *  @return  the constructed `BigDecimal`
   */
  def apply(unscaledVal: BigInt, scale: Int): BigDecimal =
    exact(new BigDec(unscaledVal.bigInteger, scale))
  
  /** Constructs a `BigDecimal` whose unscaled value is equal to that
   *  of the specified `BigInt` value.
   *
   *  @param unscaledVal the specified `BigInt` value
   *  @param scale       the scale
   *  @param mc          the precision and rounding mode for creation of this value and future operations on it   
   *  @return  the constructed `BigDecimal`
   */
  def apply(unscaledVal: BigInt, scale: Int, mc: MathContext): BigDecimal =
    new BigDecimal(new BigDec(unscaledVal.bigInteger, scale, mc), mc)

  /** Constructs a `BigDecimal` from a `java.math.BigDecimal`. */
  def apply(bd: BigDec): BigDecimal = apply(bd, defaultMathContext)
  
  @deprecated("This method appears to round a java.math.BigDecimal but actually doesn't.  Use new BigDecimal(bd, mc) instead for no rounding, or BigDecimal.decimal(bd, mc) for rounding.", "2.11")
  def apply(bd: BigDec, mc: MathContext): BigDecimal = new BigDecimal(bd, mc)

  /** Implicit conversion from `Int` to `BigDecimal`. */
  implicit def int2bigDecimal(i: Int): BigDecimal = apply(i)

  /** Implicit conversion from `Long` to `BigDecimal`. */
  implicit def long2bigDecimal(l: Long): BigDecimal = apply(l)

  /** Implicit conversion from `Double` to `BigDecimal`. */
  implicit def double2bigDecimal(d: Double): BigDecimal = decimal(d)

  /** Implicit conversion from `java.math.BigDecimal` to `scala.BigDecimal`. */
  implicit def javaBigDecimal2bigDecimal(x: BigDec): BigDecimal = apply(x)
}

/**
 *  `BigDecimal` represents decimal floating-point numbers of arbitrary precision.
 *  By default, the precision approximately matches that of IEEE 128-bit floating
 *  point numbers (34 decimal digits, `HALF_EVEN` rounding mode).  Within the range
 *  of IEEE binary128 numbers, `BigDecimal` will agree with `BigInt` for both
 *  equality and hash codes (and will agree with primitive types as well).  Beyond
 *  that range--numbers with more than 4934 digits when written out in full--the
 *  `hashCode` of `BigInt` and `BigDecimal` is allowed to diverge due to difficulty
 *  in efficiently computing both the decimal representation in `BigDecimal` and the
 *  binary representation in `BigInt`.
 *
 *  When creating a `BigDecimal` from a `Double` or `Float`, care must be taken as
 *  the binary fraction representation of `Double` and `Float` does not easily
 *  convert into a decimal representation.  Three explicit schemes are available
 *  for conversion.  `BigDecimal.decimal` will convert the floating-point number
 *  to a decimal text representation, and build a `BigDecimal` based on that.
 *  `BigDecimal.binary` will expand the binary fraction to the requested or default
 *  precision.  `BigDecimal.exact` will expand the binary fraction to the
 *  full number of digits, thus producing the exact decimal value corresponding to
 *  the binary fraction of that floating-point number.  `BigDecimal` equality
 *  matches the decimal expansion of `Double`: `BigDecimal.decimal(0.1) == 0.1`.
 *  Note that since `0.1f != 0.1`, the same is not true for `Float`.  Instead,
 *  `0.1f == BigDecimal.decimal((0.1f).toDouble)`.
 *
 *  To test whether a `BigDecimal` number can be converted to a `Double` or
 *  `Float` and then back without loss of information by using one of these
 *  methods, test with `isDecimalDouble`, `isBinaryDouble`, or `isExactDouble`
 *  or the corresponding `Float` versions.  Note that `BigInt`'s `isValidDouble`
 *  will agree with `isExactDouble`, not the `isDecimalDouble` used by default.
 *
 *  `BigDecimal` uses the decimal representation of binary floating-point numbers
 *  to determine equality and hash codes.  This yields different answers than
 *  conversion between `Long` and `Double` values, where the exact form is used.
 *  As always, since floating-point is a lossy representation, it is advisable to
 *  take care when assuming identity will be maintained across multiple conversions.
 *
 *  `BigDecimal` maintains a `MathContext` that determines the rounding that
 *  is applied to certain calculations.  In most cases, the value of the
 *  `BigDecimal` is also rounded to the precision specified by the `MathContext`.
 *  To create a `BigDecimal` with a different precision than its `MathContext`,
 *  use `new BigDecimal(new java.math.BigDecimal(...), mc)`.  Rounding will
 *  be applied on those mathematical operations that can dramatically change the
 *  number of digits in a full representation, namely multiplication, division,
 *  and powers.  The left-hand argument's `MathContext` always determines the
 *  degree of rounding, if any, and is the one propagated through arithmetic
 *  operations that do not apply rounding themselves.
 *
 *  @author  Stephane Micheloud
 *  @author  Rex Kerr
 *  @version 1.1
 */
final class BigDecimal(val bigDecimal: BigDec, val mc: MathContext)
extends ScalaNumber with ScalaNumericConversions with Serializable {
  def this(bigDecimal: BigDec) = this(bigDecimal, BigDecimal.defaultMathContext)
  import BigDecimal.RoundingMode._
  import BigDecimal.{decimal, binary, exact}
  
  if (bigDecimal eq null) throw new IllegalArgumentException("null value for BigDecimal")
  if (mc eq null) throw new IllegalArgumentException("null MathContext for BigDecimal")

  // There was an implicit to cut down on the wrapper noise for BigDec -> BigDecimal.
  // However, this may mask introduction of surprising behavior (e.g. lack of rounding
  // where one might expect it).  Wrappers should be applied explicitly with an
  // eye to correctness.

  // Sane hash code computation (which is surprisingly hard).
  // Note--not lazy val because we can't afford the extra space.
  private final var computedHashCode: Int = BigDecimal.hashCodeNotComputed
  private final def computeHashCode(): Unit = {
    computedHashCode =
      if (isWhole && (precision - scale) < BigDecimal.maximumHashScale) toBigInt.hashCode
      else if (isDecimalDouble) doubleValue.##
      else {
        val temp = bigDecimal.stripTrailingZeros
        scala.util.hashing.MurmurHash3.mixLast( temp.scaleByPowerOfTen(temp.scale).toBigInteger.hashCode, temp.scale )
      }
  }
  
  /** Returns the hash code for this BigDecimal.
   *  Note that this does not merely use the underlying java object's
   *  `hashCode` because we compare `BigDecimal`s with `compareTo`
   *  which deems 2 == 2.00, whereas in java these are unequal
   *  with unequal `hashCode`s.  These hash codes agree with `BigInt`
   *  for whole numbers up ~4934 digits (the range of IEEE 128 bit floating
   *  point).  Beyond this, hash codes will disagree; this prevents the
   *  explicit representation of the `BigInt` form for `BigDecimal` values
   *  with large exponents.
   */
  override def hashCode(): Int = {
    if (computedHashCode == BigDecimal.hashCodeNotComputed) computeHashCode
    computedHashCode
  }

  /** Compares this BigDecimal with the specified value for equality.  Where `Float` and `Double`
   *  disagree, `BigDecimal` will agree with the `Double` value
   */
  override def equals (that: Any): Boolean = that match {
    case that: BigDecimal     => this equals that
    case that: BigInt         => 
      that.bitLength > (precision-scale-2)*BigDecimal.deci2binary && 
      this.toBigIntExact.exists(that equals _)
    case that: Double         => 
      !that.isInfinity && {
        val d = toDouble
        !d.isInfinity && d == that && equals(decimal(d))
      }
    case that: Float          => 
      !that.isInfinity && {
        val f = toFloat
        !f.isInfinity && f == that && equals(decimal(f.toDouble))
      }
    case _                    => isValidLong && unifiedPrimitiveEquals(that)
  }
  override def isValidByte  = noArithmeticException(toByteExact)
  override def isValidShort = noArithmeticException(toShortExact)
  override def isValidChar  = isValidInt && toIntExact >= Char.MinValue && toIntExact <= Char.MaxValue
  override def isValidInt   = noArithmeticException(toIntExact)
  def isValidLong  = noArithmeticException(toLongExact)
  /** Tests whether the value is a valid Float.  "Valid" has several distinct meanings, however.  Use
    * `isExactFloat`, `isBinaryFloat`, or `isDecimalFloat`, depending on the intended meaning.
    * By default, `decimal` creation is used, so `isDecimalFloat` is probably what you want.
    */
  @deprecated("What constitutes validity is unclear.  Use `isExactFloat`, `isBinaryFloat`, or `isDecimalFloat` instead.", "2.11")
  def isValidFloat = {
    val f = toFloat
    !f.isInfinity && bigDecimal.compareTo(new BigDec(f.toDouble)) == 0
  }
  /** Tests whether the value is a valid Double.  "Valid" has several distinct meanings, however.  Use
    * `isExactDouble`, `isBinaryDouble`, or `isDecimalDouble`, depending on the intended meaning.
    * By default, `decimal` creation is used, so `isDecimalDouble` is probably what you want.
    */
  @deprecated("Validity has distinct meanings.  Use `isExactDouble`, `isBinaryDouble`, or `isDecimalDouble` instead.", "2.11")
  def isValidDouble = {
    val d = toDouble
    !d.isInfinity && bigDecimal.compareTo(new BigDec(d)) == 0
  }
  
  /** Tests whether this `BigDecimal` holds the decimal representation of a `Double`. */
  def isDecimalDouble = {
    val d = toDouble
    !d.isInfinity && equals(decimal(d))
  }
  
  /** Tests whether this `BigDecimal` holds the decimal representation of a `Float`. */
  def isDecimalFloat = {
    val f = toFloat
    !f.isInfinity && equals(decimal(f))
  }
  
  /** Tests whether this `BigDecimal` holds, to within precision, the binary representation of a `Double`. */
  def isBinaryDouble = {
    val d = toDouble
    !d.isInfinity && equals(binary(d,mc))
  }
  
  /** Tests whether this `BigDecimal` holds, to within precision, the binary representation of a `Float`. */
  def isBinaryFloat = {
    val f = toFloat
    !f.isInfinity && equals(binary(f,mc))
  }
  
  /** Tests whether this `BigDecimal` holds the exact expansion of a `Double`'s binary fractional form into base 10. */
  def isExactDouble = {
    val d = toDouble
    !d.isInfinity && equals(exact(d))
  }
  
  /** Tests whether this `BigDecimal` holds the exact expansion of a `Float`'s binary fractional form into base 10. */
  def isExactFloat = {
    val f = toFloat
    !f.isInfinity && equals(exact(f.toDouble))
  }
  

  private def noArithmeticException(body: => Unit): Boolean = {
    try   { body ; true }
    catch { case _: ArithmeticException => false }
  }

  def isWhole() = scale <= 0 || bigDecimal.stripTrailingZeros.scale <= 0
  
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
  def +  (that: BigDecimal): BigDecimal = new BigDecimal(this.bigDecimal add that.bigDecimal, mc)

  /** Subtraction of BigDecimals
   */
  def -  (that: BigDecimal): BigDecimal = new BigDecimal(this.bigDecimal subtract that.bigDecimal, mc)

  /** Multiplication of BigDecimals
   */
  def *  (that: BigDecimal): BigDecimal = new BigDecimal(this.bigDecimal.multiply(that.bigDecimal, mc), mc)

  /** Division of BigDecimals
   */
  def /  (that: BigDecimal): BigDecimal = new BigDecimal(this.bigDecimal.divide(that.bigDecimal, mc), mc)

  /** Division and Remainder - returns tuple containing the result of
   *  divideToIntegralValue and the remainder.  The computation is exact: no rounding is applied.
   */
  def /% (that: BigDecimal): (BigDecimal, BigDecimal) =
    this.bigDecimal.divideAndRemainder(that.bigDecimal) match {
      case Array(q, r)  => (new BigDecimal(q, mc), new BigDecimal(r, mc))
    }

  /** Divide to Integral value.
   */
  def quot (that: BigDecimal): BigDecimal =
    new BigDecimal(this.bigDecimal divideToIntegralValue that.bigDecimal, mc)

  /** Returns the minimum of this and that, or this if the two are equal
   */
  def min (that: BigDecimal): BigDecimal = (this compare that) match {
    case x if x <= 0 => this
    case _           => that
  }
  
  /** Returns the maximum of this and that, or this if the two are equal
   */
  def max (that: BigDecimal): BigDecimal = (this compare that) match {
    case x if x >= 0 => this
    case _           => that
  }
  
  /** Remainder after dividing this by that.
   */
  def remainder (that: BigDecimal): BigDecimal = new BigDecimal(this.bigDecimal remainder that.bigDecimal, mc)

  /** Remainder after dividing this by that.
   */
  def % (that: BigDecimal): BigDecimal = this remainder that

  /** Returns a BigDecimal whose value is this ** n.
   */
  def pow (n: Int): BigDecimal = new BigDecimal(this.bigDecimal.pow(n, mc), mc)

  /** Returns a BigDecimal whose value is the negation of this BigDecimal
   */
  def unary_- : BigDecimal = new BigDecimal(this.bigDecimal.negate(), mc)

  /** Returns the absolute value of this BigDecimal
   */
  def abs: BigDecimal = if (signum < 0) unary_- else this

  /** Returns the sign of this BigDecimal;
   *   -1 if it is less than 0,
   *   +1 if it is greater than 0,
   *   0  if it is equal to 0.
   */
  def signum: Int = this.bigDecimal.signum()

  /** Returns the precision of this `BigDecimal`.
   */
  def precision: Int = this.bigDecimal.precision()

  /** Returns a BigDecimal rounded according to the supplied MathContext settings, but
   *  preserving its own MathContext for future operations.
   */
  def round(mc: MathContext): BigDecimal = {
    val r = this.bigDecimal round mc
    if (r eq bigDecimal) this else new BigDecimal(r, this.mc)
  }
  
  /** Returns a `BigDecimal` rounded according to its own `MathContext` */
  def rounded: BigDecimal = {
    val r = bigDecimal round mc
    if (r eq bigDecimal) this else new BigDecimal(r, mc)
  }

  /** Returns the scale of this `BigDecimal`.
   */
  def scale: Int = this.bigDecimal.scale()

  /** Returns the size of an ulp, a unit in the last place, of this BigDecimal.
   */
  def ulp: BigDecimal = new BigDecimal(this.bigDecimal.ulp, mc)

  /** Returns a new BigDecimal based on the supplied MathContext, rounded as needed.
   */
  def apply(mc: MathContext): BigDecimal = new BigDecimal(this.bigDecimal round mc, mc)

  /** Returns a `BigDecimal` whose scale is the specified value, and whose value is
   *  numerically equal to this BigDecimal's.
   */
  def setScale(scale: Int): BigDecimal = 
    if (this.scale == scale) this
    else new BigDecimal(this.bigDecimal setScale scale, mc)

  def setScale(scale: Int, mode: RoundingMode): BigDecimal =
    if (this.scale == scale) this
    else new BigDecimal(this.bigDecimal.setScale(scale, mode.id), mc)

  /** Converts this BigDecimal to a Byte.
   *  If the BigDecimal is too big to fit in a Byte, only the low-order 8 bits are returned.
   *  Note that this conversion can lose information about the overall magnitude of the
   *  BigDecimal value as well as return a result with the opposite sign.
   */
  override def byteValue   = intValue.toByte

  /** Converts this BigDecimal to a Short.
   *  If the BigDecimal is too big to fit in a Short, only the low-order 16 bits are returned.
   *  Note that this conversion can lose information about the overall magnitude of the
   *  BigDecimal value as well as return a result with the opposite sign.
   */
  override def shortValue  = intValue.toShort

  /** Converts this BigDecimal to a Char.
   *  If the BigDecimal is too big to fit in a Char, only the low-order 16 bits are returned.
   *  Note that this conversion can lose information about the overall magnitude of the
   *  BigDecimal value and that it always returns a positive result.
   */
  def charValue   = intValue.toChar

  /** Converts this BigDecimal to an Int.
   *  If the BigDecimal is too big to fit in an Int, only the low-order 32 bits
   *  are returned. Note that this conversion can lose information about the
   *  overall magnitude of the BigDecimal value as well as return a result with
   *  the opposite sign.
   */
  def intValue    = this.bigDecimal.intValue

  /** Converts this BigDecimal to a Long.
   *  If the BigDecimal is too big to fit in a Long, only the low-order 64 bits
   *  are returned. Note that this conversion can lose information about the
   *  overall magnitude of the BigDecimal value as well as return a result with
   *  the opposite sign.
   */
  def longValue   = this.bigDecimal.longValue

  /** Converts this BigDecimal to a Float.
   *  if this BigDecimal has too great a magnitude to represent as a float,
   *  it will be converted to `Float.NEGATIVE_INFINITY` or
   *  `Float.POSITIVE_INFINITY` as appropriate.
   */
  def floatValue  = this.bigDecimal.floatValue

  /** Converts this BigDecimal to a Double.
   *  if this BigDecimal has too great a magnitude to represent as a double,
   *  it will be converted to `Double.NEGATIVE_INFINITY` or
   *  `Double.POSITIVE_INFINITY` as appropriate.
   */
  def doubleValue = this.bigDecimal.doubleValue

  /** Converts this `BigDecimal` to a [[scala.Byte]], checking for lost information.
    * If this `BigDecimal` has a nonzero fractional part, or is out of the possible
    * range for a [[scala.Byte]] result, then a `java.lang.ArithmeticException` is
    * thrown.
    */
  def toByteExact = bigDecimal.byteValueExact

  /** Converts this `BigDecimal` to a [[scala.Short]], checking for lost information.
    * If this `BigDecimal` has a nonzero fractional part, or is out of the possible
    * range for a [[scala.Short]] result, then a `java.lang.ArithmeticException` is
    * thrown.
    */
  def toShortExact = bigDecimal.shortValueExact

  /** Converts this `BigDecimal` to a [[scala.Int]], checking for lost information.
    * If this `BigDecimal` has a nonzero fractional part, or is out of the possible
    * range for an [[scala.Int]] result, then a `java.lang.ArithmeticException` is
    * thrown.
    */
  def toIntExact = bigDecimal.intValueExact

  /** Converts this `BigDecimal` to a [[scala.Long]], checking for lost information.
    * If this `BigDecimal` has a nonzero fractional part, or is out of the possible
    * range for a [[scala.Long]] result, then a `java.lang.ArithmeticException` is
    * thrown.
    */
  def toLongExact = bigDecimal.longValueExact

  /** Creates a partially constructed NumericRange[BigDecimal] in range
   *  `[start;end)`, where start is the target BigDecimal.  The step
   *  must be supplied via the "by" method of the returned object in order
   *  to receive the fully constructed range.  For example:
   * {{{
   * val partial = BigDecimal(1.0) to 2.0       // not usable yet
   * val range = partial by 0.01                // now a NumericRange
   * val range2 = BigDecimal(0) to 1.0 by 0.01  // all at once of course is fine too
   * }}}
   *
   *  @param end    the end value of the range (exclusive)
   *  @return       the partially constructed NumericRange
   */
  def until(end: BigDecimal): Range.Partial[BigDecimal, NumericRange.Exclusive[BigDecimal]] =
    new Range.Partial(until(end, _))

  /** Same as the one-argument `until`, but creates the range immediately. */
  def until(end: BigDecimal, step: BigDecimal) = Range.BigDecimal(this, end, step)

  /** Like `until`, but inclusive of the end value. */
  def to(end: BigDecimal): Range.Partial[BigDecimal, NumericRange.Inclusive[BigDecimal]] =
    new Range.Partial(to(end, _))

  /** Like `until`, but inclusive of the end value. */
  def to(end: BigDecimal, step: BigDecimal) = Range.BigDecimal.inclusive(this, end, step)

  /** Converts this `BigDecimal` to a scala.BigInt.
   */
  def toBigInt(): BigInt = new BigInt(this.bigDecimal.toBigInteger())

  /** Converts this `BigDecimal` to a scala.BigInt if it
   *  can be done losslessly, returning Some(BigInt) or None.
   */
  def toBigIntExact(): Option[BigInt] =
    if (isWhole()) {
      try Some(new BigInt(this.bigDecimal.toBigIntegerExact()))
      catch { case _: ArithmeticException => None }
    }
    else None

  /** Returns the decimal String representation of this BigDecimal.
   */
  override def toString(): String = this.bigDecimal.toString()

}
