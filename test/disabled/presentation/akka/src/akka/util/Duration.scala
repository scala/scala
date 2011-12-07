/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.util

import java.util.concurrent.TimeUnit
import TimeUnit._
import java.lang.{ Long => JLong, Double => JDouble }

object Duration {
  def apply(length: Long, unit: TimeUnit): Duration = new FiniteDuration(length, unit)
  def apply(length: Double, unit: TimeUnit): Duration = fromNanos(unit.toNanos(1) * length)
  def apply(length: Long, unit: String): Duration = new FiniteDuration(length, timeUnit(unit))

  def fromNanos(nanos: Long): Duration = {
    if (nanos % 86400000000000L == 0) {
      Duration(nanos / 86400000000000L, DAYS)
    } else if (nanos % 3600000000000L == 0) {
      Duration(nanos / 3600000000000L, HOURS)
    } else if (nanos % 60000000000L == 0) {
      Duration(nanos / 60000000000L, MINUTES)
    } else if (nanos % 1000000000L == 0) {
      Duration(nanos / 1000000000L, SECONDS)
    } else if (nanos % 1000000L == 0) {
      Duration(nanos / 1000000L, MILLISECONDS)
    } else if (nanos % 1000L == 0) {
      Duration(nanos / 1000L, MICROSECONDS)
    } else {
      Duration(nanos, NANOSECONDS)
    }
  }

  def fromNanos(nanos: Double): Duration = fromNanos((nanos + 0.5).asInstanceOf[Long])

  /**
   * Construct a Duration by parsing a String. In case of a format error, a
   * RuntimeException is thrown. See `unapply(String)` for more information.
   */
  def apply(s: String): Duration = unapply(s) getOrElse sys.error("format error")

  /**
   * Deconstruct a Duration into length and unit if it is finite.
   */
  def unapply(d: Duration): Option[(Long, TimeUnit)] = {
    if (d.finite_?) {
      Some((d.length, d.unit))
    } else {
      None
    }
  }

  private val RE = ("""^\s*(\d+(?:\.\d+)?)\s*""" + // length part
    "(?:" + // units are distinguished in separate match groups
    "(d|day|days)|" +
    "(h|hour|hours)|" +
    "(min|minute|minutes)|" +
    "(s|sec|second|seconds)|" +
    "(ms|milli|millis|millisecond|milliseconds)|" +
    "(µs|micro|micros|microsecond|microseconds)|" +
    "(ns|nano|nanos|nanosecond|nanoseconds)" +
    """)\s*$""").r // close the non-capturing group
  private val REinf = """^\s*Inf\s*$""".r
  private val REminf = """^\s*(?:-\s*|Minus)Inf\s*""".r

  /**
   * Parse String, return None if no match. Format is `"<length><unit>"`, where
   * whitespace is allowed before, between and after the parts. Infinities are
   * designated by `"Inf"` and `"-Inf"` or `"MinusInf"`.
   */
  def unapply(s: String): Option[Duration] = s match {
    case RE(length, d, h, m, s, ms, mus, ns) =>
      if (d ne null) Some(Duration(JDouble.parseDouble(length), DAYS)) else if (h ne null) Some(Duration(JDouble.parseDouble(length), HOURS)) else if (m ne null) Some(Duration(JDouble.parseDouble(length), MINUTES)) else if (s ne null) Some(Duration(JDouble.parseDouble(length), SECONDS)) else if (ms ne null) Some(Duration(JDouble.parseDouble(length), MILLISECONDS)) else if (mus ne null) Some(Duration(JDouble.parseDouble(length), MICROSECONDS)) else if (ns ne null) Some(Duration(JDouble.parseDouble(length), NANOSECONDS)) else
        sys.error("made some error in regex (should not be possible)")
    case REinf()  => Some(Inf)
    case REminf() => Some(MinusInf)
    case _        => None
  }

  /**
   * Parse TimeUnit from string representation.
   */
  def timeUnit(unit: String) = unit.toLowerCase match {
    case "d" | "day" | "days"                                       => DAYS
    case "h" | "hour" | "hours"                                     => HOURS
    case "min" | "minute" | "minutes"                               => MINUTES
    case "s" | "sec" | "second" | "seconds"                         => SECONDS
    case "ms" | "milli" | "millis" | "millisecond" | "milliseconds" => MILLISECONDS
    case "µs" | "micro" | "micros" | "microsecond" | "microseconds" => MICROSECONDS
    case "ns" | "nano" | "nanos" | "nanosecond" | "nanoseconds"     => NANOSECONDS
  }

  val Zero: Duration = new FiniteDuration(0, NANOSECONDS)

  trait Infinite {
    this: Duration =>

    override def equals(other: Any) = false

    def +(other: Duration): Duration =
      other match {
        case _: this.type => this
        case _: Infinite  => throw new IllegalArgumentException("illegal addition of infinities")
        case _            => this
      }
    def -(other: Duration): Duration =
      other match {
        case _: this.type => throw new IllegalArgumentException("illegal subtraction of infinities")
        case _            => this
      }
    def *(factor: Double): Duration = this
    def /(factor: Double): Duration = this
    def /(other: Duration): Double =
      other match {
        case _: Infinite => throw new IllegalArgumentException("illegal division of infinities")
        // maybe questionable but pragmatic: Inf / 0 => Inf
        case x           => Double.PositiveInfinity * (if ((this > Zero) ^ (other >= Zero)) -1 else 1)
      }

    def finite_? = false

    def length: Long = throw new IllegalArgumentException("length not allowed on infinite Durations")
    def unit: TimeUnit = throw new IllegalArgumentException("unit not allowed on infinite Durations")
    def toNanos: Long = throw new IllegalArgumentException("toNanos not allowed on infinite Durations")
    def toMicros: Long = throw new IllegalArgumentException("toMicros not allowed on infinite Durations")
    def toMillis: Long = throw new IllegalArgumentException("toMillis not allowed on infinite Durations")
    def toSeconds: Long = throw new IllegalArgumentException("toSeconds not allowed on infinite Durations")
    def toMinutes: Long = throw new IllegalArgumentException("toMinutes not allowed on infinite Durations")
    def toHours: Long = throw new IllegalArgumentException("toHours not allowed on infinite Durations")
    def toDays: Long = throw new IllegalArgumentException("toDays not allowed on infinite Durations")
    def toUnit(unit: TimeUnit): Double = throw new IllegalArgumentException("toUnit not allowed on infinite Durations")

    def printHMS = toString
  }

  /**
   * Infinite duration: greater than any other and not equal to any other,
   * including itself.
   */
  val Inf: Duration = new Duration with Infinite {
    override def toString = "Duration.Inf"
    def >(other: Duration) = true
    def >=(other: Duration) = true
    def <(other: Duration) = false
    def <=(other: Duration) = false
    def unary_- : Duration = MinusInf
  }

  /**
   * Infinite negative duration: lesser than any other and not equal to any other,
   * including itself.
   */
  val MinusInf: Duration = new Duration with Infinite {
    override def toString = "Duration.MinusInf"
    def >(other: Duration) = false
    def >=(other: Duration) = false
    def <(other: Duration) = true
    def <=(other: Duration) = true
    def unary_- : Duration = Inf
  }

  // Java Factories
  def create(length: Long, unit: TimeUnit): Duration = apply(length, unit)
  def create(length: Double, unit: TimeUnit): Duration = apply(length, unit)
  def create(length: Long, unit: String): Duration = apply(length, unit)
  def parse(s: String): Duration = unapply(s).get
}

/**
 * Utility for working with java.util.concurrent.TimeUnit durations.
 *
 * <p/>
 * Examples of usage from Java:
 * <pre>
 * import akka.util.FiniteDuration;
 * import java.util.concurrent.TimeUnit;
 *
 * Duration duration = new FiniteDuration(100, MILLISECONDS);
 * Duration duration = new FiniteDuration(5, "seconds");
 *
 * duration.toNanos();
 * </pre>
 *
 * <p/>
 * Examples of usage from Scala:
 * <pre>
 * import akka.util.Duration
 * import java.util.concurrent.TimeUnit
 *
 * val duration = Duration(100, MILLISECONDS)
 * val duration = Duration(100, "millis")
 *
 * duration.toNanos
 * duration < 1.second
 * duration <= Duration.Inf
 * </pre>
 *
 * <p/>
 * Implicits are also provided for Int, Long and Double. Example usage:
 * <pre>
 * import akka.util.duration._
 *
 * val duration = 100 millis
 * </pre>
 *
 * Extractors, parsing and arithmetic are also included:
 * <pre>
 * val d = Duration("1.2 µs")
 * val Duration(length, unit) = 5 millis
 * val d2 = d * 2.5
 * val d3 = d2 + 1.millisecond
 * </pre>
 */
abstract class Duration {
  def length: Long
  def unit: TimeUnit
  def toNanos: Long
  def toMicros: Long
  def toMillis: Long
  def toSeconds: Long
  def toMinutes: Long
  def toHours: Long
  def toDays: Long
  def toUnit(unit: TimeUnit): Double
  def printHMS: String
  def <(other: Duration): Boolean
  def <=(other: Duration): Boolean
  def >(other: Duration): Boolean
  def >=(other: Duration): Boolean
  def +(other: Duration): Duration
  def -(other: Duration): Duration
  def *(factor: Double): Duration
  def /(factor: Double): Duration
  def /(other: Duration): Double
  def unary_- : Duration
  def finite_? : Boolean

  // Java API
  def lt(other: Duration) = this < other
  def lteq(other: Duration) = this <= other
  def gt(other: Duration) = this > other
  def gteq(other: Duration) = this >= other
  def plus(other: Duration) = this + other
  def minus(other: Duration) = this - other
  def mul(factor: Double) = this * factor
  def div(factor: Double) = this / factor
  def div(other: Duration) = this / other
  def neg() = -this
  def isFinite() = finite_?
}

class FiniteDuration(val length: Long, val unit: TimeUnit) extends Duration {
  import Duration._

  def this(length: Long, unit: String) = this(length, Duration.timeUnit(unit))

  def toNanos = unit.toNanos(length)
  def toMicros = unit.toMicros(length)
  def toMillis = unit.toMillis(length)
  def toSeconds = unit.toSeconds(length)
  def toMinutes = unit.toMinutes(length)
  def toHours = unit.toHours(length)
  def toDays = unit.toDays(length)
  def toUnit(u: TimeUnit) = long2double(toNanos) / NANOSECONDS.convert(1, u)

  override def toString = this match {
    case Duration(1, DAYS)         => "1 day"
    case Duration(x, DAYS)         => x + " days"
    case Duration(1, HOURS)        => "1 hour"
    case Duration(x, HOURS)        => x + " hours"
    case Duration(1, MINUTES)      => "1 minute"
    case Duration(x, MINUTES)      => x + " minutes"
    case Duration(1, SECONDS)      => "1 second"
    case Duration(x, SECONDS)      => x + " seconds"
    case Duration(1, MILLISECONDS) => "1 millisecond"
    case Duration(x, MILLISECONDS) => x + " milliseconds"
    case Duration(1, MICROSECONDS) => "1 microsecond"
    case Duration(x, MICROSECONDS) => x + " microseconds"
    case Duration(1, NANOSECONDS)  => "1 nanosecond"
    case Duration(x, NANOSECONDS)  => x + " nanoseconds"
  }

  def printHMS = "%02d:%02d:%06.3f".format(toHours, toMinutes % 60, toMillis / 1000. % 60)

  def <(other: Duration) = {
    if (other.finite_?) {
      toNanos < other.asInstanceOf[FiniteDuration].toNanos
    } else {
      other > this
    }
  }

  def <=(other: Duration) = {
    if (other.finite_?) {
      toNanos <= other.asInstanceOf[FiniteDuration].toNanos
    } else {
      other >= this
    }
  }

  def >(other: Duration) = {
    if (other.finite_?) {
      toNanos > other.asInstanceOf[FiniteDuration].toNanos
    } else {
      other < this
    }
  }

  def >=(other: Duration) = {
    if (other.finite_?) {
      toNanos >= other.asInstanceOf[FiniteDuration].toNanos
    } else {
      other <= this
    }
  }

  def +(other: Duration) = {
    if (!other.finite_?) {
      other
    } else {
      val nanos = toNanos + other.asInstanceOf[FiniteDuration].toNanos
      fromNanos(nanos)
    }
  }

  def -(other: Duration) = {
    if (!other.finite_?) {
      other
    } else {
      val nanos = toNanos - other.asInstanceOf[FiniteDuration].toNanos
      fromNanos(nanos)
    }
  }

  def *(factor: Double) = fromNanos(long2double(toNanos) * factor)

  def /(factor: Double) = fromNanos(long2double(toNanos) / factor)

  def /(other: Duration) = if (other.finite_?) long2double(toNanos) / other.toNanos else 0

  def unary_- = Duration(-length, unit)

  def finite_? = true

  override def equals(other: Any) =
    other.isInstanceOf[FiniteDuration] &&
      toNanos == other.asInstanceOf[FiniteDuration].toNanos

  override def hashCode = toNanos.asInstanceOf[Int]
}

class DurationInt(n: Int) {
  def nanoseconds = Duration(n, NANOSECONDS)
  def nanos = Duration(n, NANOSECONDS)
  def nanosecond = Duration(n, NANOSECONDS)
  def nano = Duration(n, NANOSECONDS)

  def microseconds = Duration(n, MICROSECONDS)
  def micros = Duration(n, MICROSECONDS)
  def microsecond = Duration(n, MICROSECONDS)
  def micro = Duration(n, MICROSECONDS)

  def milliseconds = Duration(n, MILLISECONDS)
  def millis = Duration(n, MILLISECONDS)
  def millisecond = Duration(n, MILLISECONDS)
  def milli = Duration(n, MILLISECONDS)

  def seconds = Duration(n, SECONDS)
  def second = Duration(n, SECONDS)

  def minutes = Duration(n, MINUTES)
  def minute = Duration(n, MINUTES)

  def hours = Duration(n, HOURS)
  def hour = Duration(n, HOURS)

  def days = Duration(n, DAYS)
  def day = Duration(n, DAYS)
}

class DurationLong(n: Long) {
  def nanoseconds = Duration(n, NANOSECONDS)
  def nanos = Duration(n, NANOSECONDS)
  def nanosecond = Duration(n, NANOSECONDS)
  def nano = Duration(n, NANOSECONDS)

  def microseconds = Duration(n, MICROSECONDS)
  def micros = Duration(n, MICROSECONDS)
  def microsecond = Duration(n, MICROSECONDS)
  def micro = Duration(n, MICROSECONDS)

  def milliseconds = Duration(n, MILLISECONDS)
  def millis = Duration(n, MILLISECONDS)
  def millisecond = Duration(n, MILLISECONDS)
  def milli = Duration(n, MILLISECONDS)

  def seconds = Duration(n, SECONDS)
  def second = Duration(n, SECONDS)

  def minutes = Duration(n, MINUTES)
  def minute = Duration(n, MINUTES)

  def hours = Duration(n, HOURS)
  def hour = Duration(n, HOURS)

  def days = Duration(n, DAYS)
  def day = Duration(n, DAYS)
}

class DurationDouble(d: Double) {
  def nanoseconds = Duration(d, NANOSECONDS)
  def nanos = Duration(d, NANOSECONDS)
  def nanosecond = Duration(d, NANOSECONDS)
  def nano = Duration(d, NANOSECONDS)

  def microseconds = Duration(d, MICROSECONDS)
  def micros = Duration(d, MICROSECONDS)
  def microsecond = Duration(d, MICROSECONDS)
  def micro = Duration(d, MICROSECONDS)

  def milliseconds = Duration(d, MILLISECONDS)
  def millis = Duration(d, MILLISECONDS)
  def millisecond = Duration(d, MILLISECONDS)
  def milli = Duration(d, MILLISECONDS)

  def seconds = Duration(d, SECONDS)
  def second = Duration(d, SECONDS)

  def minutes = Duration(d, MINUTES)
  def minute = Duration(d, MINUTES)

  def hours = Duration(d, HOURS)
  def hour = Duration(d, HOURS)

  def days = Duration(d, DAYS)
  def day = Duration(d, DAYS)
}
