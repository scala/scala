/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2012, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.util

import java.util.concurrent.TimeUnit
import TimeUnit._
import java.lang.{ Double => JDouble, Long => JLong }
import language.implicitConversions

/**
 * This class stores a deadline, as obtained via `Deadline.now` or the
 * duration DSL:
 *
 * {{{
 * import scala.concurrent.util.duration._
 * 3.seconds.fromNow
 * }}}
 *
 * Its main purpose is to manage repeated attempts to achieve something (like
 * awaiting a condition) by offering the methods `hasTimeLeft` and `timeLeft`.  All
 * durations are measured according to `System.nanoTime` aka wall-time; this
 * does not take into account changes to the system clock (such as leap
 * seconds).
 */
case class Deadline private (time: FiniteDuration) extends Ordered[Deadline] {
  /**
   * Return a deadline advanced (i.e. moved into the future) by the given duration.
   */
  def +(other: FiniteDuration): Deadline = copy(time = time + other)
  /**
   * Return a deadline moved backwards (i.e. towards the past) by the given duration.
   */
  def -(other: FiniteDuration): Deadline = copy(time = time - other)
  /**
   * Calculate time difference between this and the other deadline, where the result is directed (i.e. may be negative).
   */
  def -(other: Deadline): FiniteDuration = time - other.time
  /**
   * Calculate time difference between this duration and now; the result is negative if the deadline has passed.
   *
   * '''''Note that on some systems this operation is costly because it entails a system call.'''''
   * Check `System.nanoTime` for your platform.
   */
  def timeLeft: FiniteDuration = this - Deadline.now
  /**
   * Determine whether the deadline still lies in the future at the point where this method is called.
   *
   * '''''Note that on some systems this operation is costly because it entails a system call.'''''
   * Check `System.nanoTime` for your platform.
   */
  def hasTimeLeft(): Boolean = !isOverdue()
  /**
   * Determine whether the deadline lies in the past at the point where this method is called.
   *
   * '''''Note that on some systems this operation is costly because it entails a system call.'''''
   * Check `System.nanoTime` for your platform.
   */
  def isOverdue(): Boolean = (time.toNanos - System.nanoTime()) < 0
  /**
   * The natural ordering for deadline is determined by the natural order of the underlying (finite) duration.
   */
  def compare(other: Deadline) = time compare other.time
}

object Deadline {
  /**
   * Construct a deadline due exactly at the point where this method is called. Useful for then
   * advancing it to obtain a future deadline, or for sampling the current time exactly once and
   * then comparing it to multiple deadlines (using subtraction).
   */
  def now: Deadline = Deadline(Duration(System.nanoTime, NANOSECONDS))

  /**
   * The natural ordering for deadline is determined by the natural order of the underlying (finite) duration.
   */
  implicit object DeadlineIsOrdered extends Ordering[Deadline] {
    def compare(a: Deadline, b: Deadline) = a compare b
  }
}

object Duration {
  /**
   * This implicit conversion allows the use of a Deadline in place of a Duration, which will
   * insert the time left until the deadline in its place.
   */
  implicit def timeLeft(implicit d: Deadline): Duration = d.timeLeft

  /**
   * Construct a Duration from the given length and unit. Observe that nanosecond precision may be lost if
   *
   *  - the unit is NANOSECONDS
   *  - and the length has an absolute value greater than 2^53
   *
   * Infinite inputs (and NaN) are converted into [[Duration.Inf]], [[Duration.MinusInf]] and [[Duration.Undefined]], respectively.
   *
   * @throws IllegalArgumentException if the length was finite but the resulting duration cannot be expressed as a [[FiniteDuration]]
   */ 
  def apply(length: Double, unit: TimeUnit): Duration     = fromNanos(unit.toNanos(1) * length)
  /**
   * Construct a finite duration from the given length and time unit. The unit given is retained
   * throughout calculations as long as possible, so that it can be retrieved later.
   */
  def apply(length: Long, unit: TimeUnit): FiniteDuration = new FiniteDuration(length, unit)
  /**
   * Construct a finite duration from the given length and time unit, where the latter is
   * looked up in a list of string representation. Valid choices are:
   *
   * `d, day, h, hour, min, minute, s, sec, second, ms, milli, millisecond, µs, micro, microsecond, ns, nano, nanosecond`
   * and their pluralized forms (for every but the first mentioned form of each unit, i.e. no "ds", but "days").
   */
  def apply(length: Long, unit: String): FiniteDuration   = new FiniteDuration(length,  Duration.timeUnit(unit))

  // Double stores 52 bits mantissa, but there is an implied '1' in front, making the limit 2^53
  final val maxPreciseDouble = 9007199254740992d 

  /**
   * Parse String into Duration.  Format is `"<length><unit>"`, where
   * whitespace is allowed before, between and after the parts. Infinities are
   * designated by `"Inf"`, `"PlusInf"`, `"+Inf"` and `"-Inf"` or `"MinusInf"`.
   *
   * @throws NumberFormatException if format is not parseable
   */
  def apply(s: String): Duration = {
    val s1: String = s filterNot (_.isWhitespace)
    s1 match {
      case "Inf" | "PlusInf" | "+Inf" => Inf
      case "MinusInf" | "-Inf"        => MinusInf
      case _                          =>
        val unitName = s1.reverse takeWhile (_.isLetter) reverse;
        timeUnit get unitName match {
          case Some(unit) =>
            val valueStr = s1 dropRight unitName.length
            val valueD = JDouble.parseDouble(valueStr)
            if (valueD >= -maxPreciseDouble && valueD <= maxPreciseDouble) Duration(valueD, unit)
            else Duration(JLong.parseLong(valueStr), unit)
          case _          => throw new NumberFormatException("format error " + s)
        }
    }
  }

  // "ms milli millisecond" -> List("ms", "milli", "millis", "millisecond", "milliseconds")
  private[this] def words(s: String) = (s.trim split "\\s+").toList
  private[this] def expandLabels(labels: String): List[String] = {
    val hd :: rest = words(labels)
    hd :: rest.flatMap(s => List(s, s + "s"))
  }
  private[this] val timeUnitLabels = List(
    DAYS         -> "d day",
    HOURS        -> "h hour",
    MINUTES      -> "min minute",
    SECONDS      -> "s sec second",
    MILLISECONDS -> "ms milli millisecond",
    MICROSECONDS -> "µs micro microsecond",
    NANOSECONDS  -> "ns nano nanosecond"
  )

  // TimeUnit => standard label
  protected[util] val timeUnitName: Map[TimeUnit, String] =
    timeUnitLabels.toMap mapValues (s => words(s).last) toMap

  // Label => TimeUnit
  protected[util] val timeUnit: Map[String, TimeUnit] =
    timeUnitLabels flatMap { case (unit, names) => expandLabels(names) map (_ -> unit) } toMap

  /**
   * Extract length and time unit out of a string, where the format must match the description for [[Duration$.apply(String):Duration apply(String)]].
   * The extractor will not match for malformed strings or non-finite durations.
   */
  def unapply(s: String): Option[(Long, TimeUnit)] =
    ( try Some(apply(s)) catch { case _: RuntimeException => None } ) flatMap unapply

  /**
   * Extract length and time unit out of a duration, if it is finite.
   */
  def unapply(d: Duration): Option[(Long, TimeUnit)] =
    if (d.isFinite) Some((d.length, d.unit)) else None

  /**
   * Construct a possibly infinite or undefined Duration from the given number of nanoseconds.
   * 
   *  - `Double.PositiveInfinity` is mapped to [[Duration.Inf]]
   *  - `Double.NegativeInfinity` is mapped to [[Duration.MinusInf]]
   *  - `Double.NaN` is mapped to [[Duration.Undefined]]
   *  - `-0d` is mapped to [[Duration.Zero]] (exactly like `0d`)
   *
   * The semantics of the resulting Duration objects matches the semantics of their Double 
   * counterparts with respect to arithmetic operations.
   *
   * @throws IllegalArgumentException if the length was finite but the resulting duration cannot be expressed as a [[FiniteDuration]]
   */
  def fromNanos(nanos: Double): Duration = {
    if (nanos.isInfinite)
      if (nanos > 0) Inf else MinusInf
    else if (nanos.isNaN)
      Undefined
    else if (nanos > Long.MaxValue || nanos < Long.MinValue)
      throw new IllegalArgumentException("trying to construct too large duration with " + nanos + "ns")
    else
      fromNanos((nanos + 0.5).toLong)
  }

  private[this] final val  µs_per_ns = 1000L
  private[this] final val  ms_per_ns =  µs_per_ns * 1000
  private[this] final val   s_per_ns =  ms_per_ns * 1000
  private[this] final val min_per_ns =   s_per_ns * 60
  private[this] final val   h_per_ns = min_per_ns * 60
  private[this] final val   d_per_ns =   h_per_ns * 24

  /**
   * Construct a finite duration from the given number of nanoseconds. The
   * result will have the coarsest possible time unit which can exactly express
   * this duration.
   *
   * @throws IllegalArgumentException for `Long.MinValue` since that would lead to inconsistent behavior afterwards (cannot be negated)
   */
  def fromNanos(nanos: Long): FiniteDuration = {
         if (nanos %   d_per_ns == 0) Duration(nanos /   d_per_ns, DAYS)
    else if (nanos %   h_per_ns == 0) Duration(nanos /   h_per_ns, HOURS)
    else if (nanos % min_per_ns == 0) Duration(nanos / min_per_ns, MINUTES)
    else if (nanos %   s_per_ns == 0) Duration(nanos /   s_per_ns, SECONDS)
    else if (nanos %  ms_per_ns == 0) Duration(nanos /  ms_per_ns, MILLISECONDS)
    else if (nanos %  µs_per_ns == 0) Duration(nanos /  µs_per_ns, MICROSECONDS)
    else Duration(nanos, NANOSECONDS)
  }

  /**
   * Preconstructed value of `0.days`.
   */
  // unit as coarse as possible to keep (_ + Zero) sane unit-wise
  val Zero: FiniteDuration = new FiniteDuration(0, DAYS)

  /**
   * The Undefined value corresponds closely to Double.NaN:
   *
   *  - it is the result of otherwise invalid operations
   *  - it does not equal itself (according to `equals()`)
   *  - it compares greater than any other Duration apart from itself (for which `compare` returns 0)
   *
   * The particular comparison semantics mirror those of Double.NaN.
   *
   * '''''Use `eq` when checking an input of a method against this value.'''''
   */
  val Undefined: Infinite = new Infinite {
    override def toString = "Duration.Undefined"
    override def equals(other: Any) = false
    override def +(other: Duration): Duration = this
    override def -(other: Duration): Duration = this
    override def *(factor: Double): Duration  = this
    override def /(factor: Double): Duration  = this
    override def /(other: Duration): Double   = Double.NaN
    def compare(other: Duration) = if (other eq this) 0 else 1
    def unary_- : Duration = this
    def toUnit(unit: TimeUnit): Double = Double.NaN
  }

  sealed abstract class Infinite extends Duration {
    def +(other: Duration): Duration = other match {
      case x if x eq Undefined      => Undefined
      case x: Infinite if x ne this => Undefined
      case _                        => this
    }
    def -(other: Duration): Duration = other match {
      case x if x eq Undefined      => Undefined
      case x: Infinite if x eq this => Undefined
      case _                        => this
    }

    def *(factor: Double): Duration =
      if (factor == 0d || factor.isNaN) Undefined
      else if (factor < 0d) -this
      else this
    def /(factor: Double): Duration =
      if (factor.isNaN || factor.isInfinite) Undefined
      else if ((factor compare 0d) < 0) -this
      else this
    def /(other: Duration): Double = other match {
      case _: Infinite => Double.NaN
      case x           => Double.PositiveInfinity * (if ((this > Zero) ^ (other >= Zero)) -1 else 1)
    }

    final def isFinite() = false

    private[this] def fail(what: String) = throw new IllegalArgumentException(s"$what not allowed on infinite Durations")
    final def length: Long    = fail("length")
    final def unit: TimeUnit  = fail("unit")
    final def toNanos: Long   = fail("toNanos")
    final def toMicros: Long  = fail("toMicros")
    final def toMillis: Long  = fail("toMillis")
    final def toSeconds: Long = fail("toSeconds")
    final def toMinutes: Long = fail("toMinutes")
    final def toHours: Long   = fail("toHours")
    final def toDays: Long    = fail("toDays")
  }

  /**
   * Infinite duration: greater than any other (apart from Undefined) and not equal to any other
   * but itself. This value closely corresponds to Double.PositiveInfinity, 
   * matching its semantics in arithmetic operations.
   */
  val Inf: Infinite = new Infinite {
    override def toString = "Duration.Inf"
    def compare(other: Duration) = other match {
      case x if x eq Undefined => -1 // Undefined != Undefined
      case x if x eq this      => 0  // `case Inf` will include null checks in the byte code
      case _                   => 1
    }
    def unary_- : Duration = MinusInf
    def toUnit(unit: TimeUnit): Double = Double.PositiveInfinity
  }

  /**
   * Infinite duration: less than any other and not equal to any other
   * but itself. This value closely corresponds to Double.NegativeInfinity, 
   * matching its semantics in arithmetic operations.
   */
  val MinusInf: Infinite = new Infinite {
    override def toString = "Duration.MinusInf"
    def compare(other: Duration) = if (other eq this) 0 else -1
    def unary_- : Duration = Inf
    def toUnit(unit: TimeUnit): Double = Double.NegativeInfinity
  }

  // Java Factories

  /**
   * Construct a finite duration from the given length and time unit. The unit given is retained
   * throughout calculations as long as possible, so that it can be retrieved later.
   */
  def create(length: Long, unit: TimeUnit): FiniteDuration = apply(length, unit)
  /**
   * Construct a Duration from the given length and unit. Observe that nanosecond precision may be lost if
   *
   *  - the unit is NANOSECONDS
   *  - and the length has an absolute value greater than 2^53
   *
   * Infinite inputs (and NaN) are converted into [[Duration.Inf]], [[Duration.MinusInf]] and [[Duration.Undefined]], respectively.
   *
   * @throws IllegalArgumentException if the length was finite but the resulting duration cannot be expressed as a [[FiniteDuration]]
   */ 
  def create(length: Double, unit: TimeUnit): Duration     = apply(length, unit)
  /**
   * Construct a finite duration from the given length and time unit, where the latter is
   * looked up in a list of string representation. Valid choices are:
   *
   * `d, day, h, hour, min, minute, s, sec, second, ms, milli, millisecond, µs, micro, microsecond, ns, nano, nanosecond`
   * and their pluralized forms (for every but the first mentioned form of each unit, i.e. no "ds", but "days").
   */
  def create(length: Long, unit: String): FiniteDuration   = apply(length, unit)
  /**
   * Parse String into Duration.  Format is `"<length><unit>"`, where
   * whitespace is allowed before, between and after the parts. Infinities are
   * designated by `"Inf"`, `"PlusInf"`, `"+Inf"` and `"-Inf"` or `"MinusInf"`.
   *
   * @throws NumberFormatException if format is not parseable
   */
  def create(s: String): Duration                          = apply(s)

  /**
   * The natural ordering of durations matches the natural ordering for Double, including non-finite values.
   */
  implicit object DurationIsOrdered extends Ordering[Duration] {
    def compare(a: Duration, b: Duration) = a compare b
  }
}

/**
 * <h2>Utility for working with java.util.concurrent.TimeUnit durations.</h2>
 *
 * '''''This class is not meant as a general purpose representation of time, it is
 * optimized for the needs of `scala.concurrent`.'''''
 *
 * <h2>Basic Usage</h2>
 *
 * <p/>
 * Examples:
 * {{{
 * import scala.concurrent.util.Duration
 * import java.util.concurrent.TimeUnit
 *
 * val duration = Duration(100, MILLISECONDS)
 * val duration = Duration(100, "millis")
 *
 * duration.toNanos
 * duration < 1.second
 * duration <= Duration.Inf
 * }}}
 * 
 * '''''Invoking inexpressible conversions (like calling `toSeconds` on an infinite duration) will throw an IllegalArgumentException.'''''
 *
 * <p/>
 * Implicits are also provided for Int, Long and Double. Example usage:
 * {{{
 * import scala.concurrent.util.Duration._
 *
 * val duration = 100 millis
 * }}}
 *
 * '''''The DSL provided by the implicit conversions always allows construction of finite durations, even for infinite Double inputs; use Duration.Inf instead.'''''
 *
 * Extractors, parsing and arithmetic are also included:
 * {{{
 * val d = Duration("1.2 µs")
 * val Duration(length, unit) = 5 millis
 * val d2 = d * 2.5
 * val d3 = d2 + 1.millisecond
 * }}}
 *
 * <h2>Handling of Time Units</h2>
 *
 * Calculations performed on finite durations always retain the more precise unit of either operand, no matter
 * whether a coarser unit would be able to exactly express the same duration. This means that Duration can be
 * used as a lossless container for a (length, unit) pair if it is constructed using the corresponding methods
 * and no arithmetic is performed on it; adding/subtracting durations should in that case be done with care.
 *
 * <h2>Correspondence to Double Semantics</h2>
 *
 * The semantics of arithmetic operations on Duration are two-fold:
 *
 *  - exact addition/subtraction with nanosecond resolution for finite durations, independent of the summands' magnitude
 *  - isomorphic to `java.lang.Double` when it comes to infinite or undefined values
 *
 * The conversion between Duration and Double is done using [[Duration.toUnit]] (with unit NANOSECONDS) 
 * and [[Duration$.fromNanos(Double):Duration Duration.fromNanos(Double)]].
 *
 * <h2>Ordering</h2>
 *
 * The default ordering is consistent with the ordering of Double numbers, which means that Undefined is
 * considered greater than all other durations, including [[Duration.Inf]].
 *
 * @define exc @throws IllegalArgumentException when invoked on a non-finite duration
 *
 * @define ovf @throws IllegalArgumentException in case of a finite overflow: the range of a finite duration is +-(2^63-1)ns, and no conversion to infinite durations takes place.
 */
sealed abstract class Duration extends Serializable with Ordered[Duration] {
  /**
   * Obtain the length of this Duration measured in the unit obtained by the `unit` method.
   *
   * $exc
   */
  def length: Long
  /**
   * Obtain the time unit in which the length of this duration is measured.
   *
   * $exc
   */
  def unit: TimeUnit
  /**
   * Return the length of this duration measured in whole nanoseconds, rounding towards zero.
   *
   * $exc
   */
  def toNanos: Long
  /**
   * Return the length of this duration measured in whole microseconds, rounding towards zero.
   *
   * $exc
   */
  def toMicros: Long
  /**
   * Return the length of this duration measured in whole milliseconds, rounding towards zero.
   *
   * $exc
   */
  def toMillis: Long
  /**
   * Return the length of this duration measured in whole seconds, rounding towards zero.
   *
   * $exc
   */
  def toSeconds: Long
  /**
   * Return the length of this duration measured in whole minutes, rounding towards zero.
   *
   * $exc
   */
  def toMinutes: Long
  /**
   * Return the length of this duration measured in whole hours, rounding towards zero.
   *
   * $exc
   */
  def toHours: Long
  /**
   * Return the length of this duration measured in whole days, rounding towards zero.
   *
   * $exc
   */
  def toDays: Long
  /**
   * Return the number of nanoseconds as floating point number, scaled down to the given unit.
   * The result may not precisely represent this duration due to the Double datatype's inherent
   * limitations (mantissa size effectively 53 bits). Non-finite durations are represented as
   *  - [[Duration.Undefined]] is mapped to Double.NaN
   *  - [[Duration.Inf]] is mapped to Double.PositiveInfinity
   *  - [[Duration.MinusInf]] is mapped to Double.NegativeInfinity
   */
  def toUnit(unit: TimeUnit): Double

  /**
   * Return the sum of that duration and this. When involving non-finite summands the semantics match those
   * of Double.
   *
   * $ovf
   */
  def +(other: Duration): Duration
  /**
   * Return the difference of that duration and this. When involving non-finite summands the semantics match those
   * of Double.
   *
   * $ovf
   */
  def -(other: Duration): Duration
  /**
   * Return this duration multiplied by the scalar factor. When involving non-finite factors the semantics match those
   * of Double.
   *
   * $ovf
   */
  def *(factor: Double): Duration
  /**
   * Return this duration divided by the scalar factor. When involving non-finite factors the semantics match those
   * of Double.
   *
   * $ovf
   */
  def /(factor: Double): Duration
  /**
   * Return the quotient of this and that duration as floating-point number. The semantics are 
   * determined by Double as if calculating the quotient of the nanosecond lengths of both factors.
   */
  def /(other: Duration): Double
  /**
   * Negate this duration. The only two values which are mapped to themselves are [[Duration.Zero]] and [[Duration.Undefined]].
   */
  def unary_- : Duration
  /**
   * This method returns whether this duration is finite, which is not the same as 
   * `!isInfinite` for Double because this method also returns `false` for [[Duration.Undefined]].
   */
  def isFinite(): Boolean
  /**
   * Return the smaller of this and that duration as determined by the natural ordering.
   */
  def min(other: Duration): Duration = if (this < other) this else other
  /**
   * Return the larger of this and that duration as determined by the natural ordering.
   */
  def max(other: Duration): Duration = if (this > other) this else other

  // Java API

  /**
   * Return this duration divided by the scalar factor. When involving non-finite factors the semantics match those
   * of Double.
   *
   * $ovf
   */
  def div(factor: Double)    = this / factor
  /**
   * Return the quotient of this and that duration as floating-point number. The semantics are 
   * determined by Double as if calculating the quotient of the nanosecond lengths of both factors.
   */
  def div(other: Duration)   = this / other
  def gt(other: Duration)    = this > other
  def gteq(other: Duration)  = this >= other
  def lt(other: Duration)    = this < other
  def lteq(other: Duration)  = this <= other
  /**
   * Return the difference of that duration and this. When involving non-finite summands the semantics match those
   * of Double.
   *
   * $ovf
   */
  def minus(other: Duration) = this - other
  /**
   * Return this duration multiplied by the scalar factor. When involving non-finite factors the semantics match those
   * of Double.
   *
   * $ovf
   */
  def mul(factor: Double)    = this * factor
  /**
   * Negate this duration. The only two values which are mapped to themselves are [[Duration.Zero]] and [[Duration.Undefined]].
   */
  def neg()                  = -this
  /**
   * Return the sum of that duration and this. When involving non-finite summands the semantics match those
   * of Double.
   *
   * $ovf
   */
  def plus(other: Duration)  = this + other
}

object FiniteDuration {
  implicit object FiniteDurationIsOrdered extends Ordering[FiniteDuration] {
    def compare(a: FiniteDuration, b: FiniteDuration) = a compare b
  }

  def apply(length: Long, unit: TimeUnit) = new FiniteDuration(length, unit)
  def apply(length: Long, unit: String)   = new FiniteDuration(length, Duration.timeUnit(unit))

  // limit on abs. value of durations in their units
  private final val max_ns = Long.MaxValue
  private final val max_µs = max_ns  / 1000
  private final val max_ms = max_µs  / 1000
  private final val max_s  = max_ms  / 1000
  private final val max_min= max_s   / 60
  private final val max_h  = max_min / 60
  private final val max_d  = max_h   / 24
}

/**
 * This class represents a finite duration. Its addition and subtraction operators are overloaded to retain
 * this guarantee statically. The range of this class is limited to +-(2^63-1)ns, which is roughly 292 years.
 */
final class FiniteDuration(val length: Long, val unit: TimeUnit) extends Duration {
  import FiniteDuration._
  import Duration._

  private[this] def bounded(max: Long) = -max <= length && length <= max

  require(unit match {
      /*
       * enforce the 2^63-1 ns limit, must be pos/neg symmetrical because of unary_-
       */
      case NANOSECONDS  ⇒ bounded(max_ns)
      case MICROSECONDS ⇒ bounded(max_µs)
      case MILLISECONDS ⇒ bounded(max_ms)
      case SECONDS      ⇒ bounded(max_s)
      case MINUTES      ⇒ bounded(max_min)
      case HOURS        ⇒ bounded(max_h)
      case DAYS         ⇒ bounded(max_d)
      case _ ⇒
        val v = DAYS.convert(length, unit)
        -max_d <= v && v <= max_d
    }, "Duration is limited to +-(2^63-1)ns (ca. 292 years)")

  def toNanos   = unit.toNanos(length)
  def toMicros  = unit.toMicros(length)
  def toMillis  = unit.toMillis(length)
  def toSeconds = unit.toSeconds(length)
  def toMinutes = unit.toMinutes(length)
  def toHours   = unit.toHours(length)
  def toDays    = unit.toDays(length)
  def toUnit(u: TimeUnit) = toNanos.toDouble / NANOSECONDS.convert(1, u)

  /**
   * Construct a [[Deadline]] from this duration by adding it to the current instant `Deadline.now`.
   */
  def fromNow: Deadline = Deadline.now + this

  private[this] def unitString = timeUnitName(unit) + ( if (length == 1) "" else "s" )
  override def toString = "" + length + " " + unitString

  def compare(other: Duration) = other match {
    case x: FiniteDuration => toNanos compare x.toNanos
    case _                 => -(other compare this)
  }

  // see https://www.securecoding.cert.org/confluence/display/java/NUM00-J.+Detect+or+prevent+integer+overflow
  private[this] def safeAdd(a: Long, b: Long): Long = {
    if ((b > 0) && (a > Long.MaxValue - b) ||
        (b < 0) && (a < Long.MinValue - b)) throw new IllegalArgumentException("integer overflow")
    a + b
  }
  private[this] def add(otherLength: Long, otherUnit: TimeUnit): FiniteDuration = {
    val commonUnit = if (otherUnit.convert(1, unit) == 0) unit else otherUnit
    val totalLength = safeAdd(commonUnit.convert(length, unit), commonUnit.convert(otherLength, otherUnit))
    new FiniteDuration(totalLength, commonUnit)
  }

  def +(other: Duration) = other match {
    case x: FiniteDuration => add(x.length, x.unit)
    case _                 => other
  }
  def -(other: Duration) = other match {
    case x: FiniteDuration => add(-x.length, x.unit)
    case _                 => other
  }

  def *(factor: Double) = 
    if (!factor.isInfinite) fromNanos(toNanos * factor)
    else if (factor.isNaN) Undefined
    else if ((factor > 0) ^ (this < Zero)) Inf
    else MinusInf

  def /(factor: Double) =
    if (!factor.isInfinite) fromNanos(toNanos / factor)
    else if (factor.isNaN) Undefined
    else Zero

  // if this is made a constant, then scalac will elide the conditional and always return +0.0, SI-6331
  private[this] def minusZero = -0d
  def /(other: Duration): Double =
    if (other.isFinite) toNanos.toDouble / other.toNanos
    else if (other eq Undefined) Double.NaN
    else if ((length < 0) ^ (other > Zero)) 0d
    else minusZero

  // overridden methods taking FiniteDurations, so that you can calculate while statically staying finite
  def +(other: FiniteDuration) = add(other.length, other.unit)
  def -(other: FiniteDuration) = add(-other.length, other.unit)
  def plus(other: FiniteDuration) = this + other
  def minus(other: FiniteDuration) = this - other
  override def div(factor: Double) = this / factor
  override def mul(factor: Double) = this * factor
  def min(other: FiniteDuration) = if (this < other) this else other
  def max(other: FiniteDuration) = if (this > other) this else other

  def unary_- = Duration(-length, unit)

  final def isFinite() = true

  override def equals(other: Any) = other match {
    case x: FiniteDuration => toNanos == x.toNanos
    case _                 => super.equals(other)
  }
  override def hashCode = toNanos.toInt
}

trait DurationConversions extends Any {
  import duration.Classifier
  protected def durationIn(unit: TimeUnit): FiniteDuration

  def nanoseconds  = durationIn(NANOSECONDS)
  def nanos        = nanoseconds
  def nanosecond   = nanoseconds
  def nano         = nanoseconds

  def microseconds = durationIn(MICROSECONDS)
  def micros       = microseconds
  def microsecond  = microseconds
  def micro        = microseconds

  def milliseconds = durationIn(MILLISECONDS)
  def millis       = milliseconds
  def millisecond  = milliseconds
  def milli        = milliseconds

  def seconds      = durationIn(SECONDS)
  def second       = seconds

  def minutes      = durationIn(MINUTES)
  def minute       = minutes

  def hours        = durationIn(HOURS)
  def hour         = hours

  def days         = durationIn(DAYS)
  def day          = days

  def nanoseconds[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(nanoseconds)
  def nanos[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = nanoseconds(c)
  def nanosecond[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = nanoseconds(c)
  def nano[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = nanoseconds(c)

  def microseconds[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(microseconds)
  def micros[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = microseconds(c)
  def microsecond[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = microseconds(c)
  def micro[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = microseconds(c)

  def milliseconds[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(milliseconds)
  def millis[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = milliseconds(c)
  def millisecond[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = milliseconds(c)
  def milli[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = milliseconds(c)

  def seconds[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(seconds)
  def second[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = seconds(c)

  def minutes[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(minutes)
  def minute[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = minutes(c)

  def hours[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(hours)
  def hour[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = hours(c)

  def days[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(days)
  def day[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = days(c)
}

final class DurationInt(val n: Int) extends AnyVal with DurationConversions {
  override protected def durationIn(unit: TimeUnit): FiniteDuration = Duration(n, unit)
}

final class DurationLong(val n: Long) extends AnyVal with DurationConversions {
  override protected def durationIn(unit: TimeUnit): FiniteDuration = Duration(n, unit)
}

final class DurationDouble(val d: Double) extends AnyVal with DurationConversions {
  override protected def durationIn(unit: TimeUnit): FiniteDuration =
    Duration(d, unit) match {
      case f: FiniteDuration => f
      case _ => throw new IllegalArgumentException("Duration DSL not applicable to " + d)
    }
}
