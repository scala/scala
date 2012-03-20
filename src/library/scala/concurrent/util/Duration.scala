/**
 * Copyright (C) 2009-2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.concurrent.util

import java.util.concurrent.TimeUnit
import TimeUnit._
import java.lang.{ Double ⇒ JDouble }

object DurationImplicits {
  trait Classifier[C] {
    type R
    def convert(d: FiniteDuration): R
  }

  object span
  implicit object spanConvert extends Classifier[span.type] {
    type R = FiniteDuration
    def convert(d: FiniteDuration) = d
  }

  object fromNow
  implicit object fromNowConvert extends Classifier[fromNow.type] {
    type R = Deadline
    def convert(d: FiniteDuration) = Deadline.now + d
  }

  implicit def intToDurationInt(n: Int) = new DurationInt(n)
  implicit def longToDurationLong(n: Long) = new DurationLong(n)
  implicit def doubleToDurationDouble(d: Double) = new DurationDouble(d)

  implicit def pairIntToDuration(p: (Int, TimeUnit)) = Duration(p._1, p._2)
  implicit def pairLongToDuration(p: (Long, TimeUnit)) = Duration(p._1, p._2)
  implicit def durationToPair(d: Duration) = (d.length, d.unit)

  /*
   * Avoid reflection based invocation by using non-duck type
   */
  class IntMult(i: Int) {
    def *(d: Duration) = d * i
  }
  implicit def intMult(i: Int) = new IntMult(i)

  class LongMult(l: Long) {
    def *(d: Duration) = d * l
  }
  implicit def longMult(l: Long) = new LongMult(l)

  class DoubleMult(f: Double) {
    def *(d: Duration) = d * f
  }
  implicit def doubleMult(f: Double) = new DoubleMult(f)
}

case class Deadline private (time: Duration) {
  def +(other: Duration): Deadline = copy(time = time + other)
  def -(other: Duration): Deadline = copy(time = time - other)
  def -(other: Deadline): Duration = time - other.time
  def timeLeft: Duration = this - Deadline.now
  def hasTimeLeft(): Boolean = !isOverdue()
  def isOverdue(): Boolean = (time.toNanos - System.nanoTime()) < 0
}

object Deadline {
  def now: Deadline = Deadline(Duration(System.nanoTime, NANOSECONDS))
}

object Duration {
  implicit def timeLeft(implicit d: Deadline): Duration = d.timeLeft

  def apply(length: Long, unit: TimeUnit): FiniteDuration = new FiniteDuration(length, unit)
  def apply(length: Double, unit: TimeUnit): FiniteDuration = fromNanos(unit.toNanos(1) * length)
  def apply(length: Long, unit: String): FiniteDuration = {
    val (mult, timeUnit) = Duration.timeUnit(unit)
    new FiniteDuration(length * mult, timeUnit)
  }

  /**
   * Construct a Duration by parsing a String. In case of a format error, a
   * RuntimeException is thrown. See `unapply(String)` for more information.
   */
  def apply(s: String): Duration = unapply(s) getOrElse sys.error("format error " + s)

  private val RE = ("""^\s*([\+|-]?\d+(?:\.\d+)?)\s*""" + // length part
    "(?:" + // units are distinguished in separate match groups
    "(d|day|days)|" +
    "(h|hour|hours)|" +
    "(min|minute|minutes)|" +
    "(s|sec|second|seconds)|" +
    "(ms|milli|millis|millisecond|milliseconds)|" +
    "(µs|micro|micros|microsecond|microseconds)|" +
    "(ns|nano|nanos|nanosecond|nanoseconds)" +
    """)\s*$""").r // close the non-capturing group
  private val REinf = """^\s*(?:\+|Plus)?Inf\s*$""".r
  private val REminf = """^\s*(?:-|Minus)Inf\s*""".r

  /**
   * Deconstruct a Duration into `Long` length and [[java.util.concurrent.TimeUnit]] if it is a 
   * [[scala.util.concurrent.FiniteDuration]].
   *
   * @param d Duration to be deconstructed.
   */
  def unapply(d: Duration): Option[(Long, TimeUnit)] = {
    if (d.finite_?) {
      Some((d.length, d.unit))
    } else {
      None
    }
  }

  /**
   * Parse String, return None if no match. Format is `"<length><unit>"`, where
   * whitespace is allowed before, between and after the parts. Infinities are
   * designated by `"Inf"`, `"PlusInf"`, `"+Inf"` and `"-Inf"` or `"MinusInf"`.
   */
  def unapply(s: String): Option[Duration] = s match {
    case RE(length, d, h, m, s, ms, mus, ns) ⇒
      if (d ne null)
        Some(Duration(JDouble.parseDouble(length) * 86400, SECONDS))
      else if (h ne null)
        Some(Duration(JDouble.parseDouble(length) * 3600, SECONDS))
      else if (m ne null)
        Some(Duration(JDouble.parseDouble(length) * 60, SECONDS))
      else if (s ne null)
        Some(Duration(JDouble.parseDouble(length), SECONDS))
      else if (ms ne null)
        Some(Duration(JDouble.parseDouble(length), MILLISECONDS))
      else if (mus ne null)
        Some(Duration(JDouble.parseDouble(length), MICROSECONDS))
      else if (ns ne null)
        Some(Duration(JDouble.parseDouble(length), NANOSECONDS))
      else
        sys.error("made some error in regex (should not be possible)")
    case REinf()  ⇒ Some(Inf)
    case REminf() ⇒ Some(MinusInf)
    case _        ⇒ None
  }

  def fromNanos(nanos: Double): FiniteDuration =
    fromNanos((nanos + 0.5).asInstanceOf[Long])

  def fromNanos(nanos: Long): FiniteDuration = {
    if (nanos % 86400000000000L == 0) {
      Duration(nanos / 1000000000L, SECONDS)
    } else if (nanos % 1000000000L == 0) {
      Duration(nanos / 1000000000L, SECONDS)
    } else if (nanos % 1000000000L == 0) {
      Duration(nanos / 1000000000L, SECONDS)
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

  /**
   * Parse TimeUnit from string representation.
   */
  protected[util] def timeUnit(unit: String): (Long, TimeUnit) = unit.toLowerCase match {
    case "d" | "day" | "days"                                       ⇒ (86400, SECONDS)
    case "h" | "hour" | "hours"                                     ⇒ (3600, SECONDS)
    case "min" | "minute" | "minutes"                               ⇒ (60, SECONDS)
    case "s" | "sec" | "second" | "seconds"                         ⇒ (1, SECONDS)
    case "ms" | "milli" | "millis" | "millisecond" | "milliseconds" ⇒ (1, MILLISECONDS)
    case "µs" | "micro" | "micros" | "microsecond" | "microseconds" ⇒ (1, MICROSECONDS)
    case "ns" | "nano" | "nanos" | "nanosecond" | "nanoseconds"     ⇒ (1, NANOSECONDS)
  }

  val Zero: FiniteDuration = new FiniteDuration(0, NANOSECONDS)
  val Undefined: Duration = new Duration with Infinite {
    override def toString = "Duration.Undefined"
    override def equals(other: Any) = other.asInstanceOf[AnyRef] eq this
    override def +(other: Duration): Duration = throw new IllegalArgumentException("cannot add Undefined duration")
    override def -(other: Duration): Duration = throw new IllegalArgumentException("cannot subtract Undefined duration")
    override def *(factor: Double): Duration = throw new IllegalArgumentException("cannot multiply Undefined duration")
    override def /(factor: Double): Duration = throw new IllegalArgumentException("cannot divide Undefined duration")
    override def /(other: Duration): Double = throw new IllegalArgumentException("cannot divide Undefined duration")
    def compare(other: Duration) = throw new IllegalArgumentException("cannot compare Undefined duration")
    def unary_- : Duration = throw new IllegalArgumentException("cannot negate Undefined duration")
  }

  trait Infinite {
    this: Duration ⇒

    def +(other: Duration): Duration =
      other match {
        case _: this.type ⇒ this
        case _: Infinite  ⇒ throw new IllegalArgumentException("illegal addition of infinities")
        case _            ⇒ this
      }
    def -(other: Duration): Duration =
      other match {
        case _: this.type ⇒ throw new IllegalArgumentException("illegal subtraction of infinities")
        case _            ⇒ this
      }
    def *(factor: Double): Duration = this
    def /(factor: Double): Duration = this
    def /(other: Duration): Double =
      other match {
        case _: Infinite ⇒ throw new IllegalArgumentException("illegal division of infinities")
        // maybe questionable but pragmatic: Inf / 0 => Inf
        case x           ⇒ Double.PositiveInfinity * (if ((this > Zero) ^ (other >= Zero)) -1 else 1)
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

  }

  /**
   * Infinite duration: greater than any other and not equal to any other,
   * including itself.
   */
  val Inf: Duration = new Duration with Infinite {
    override def toString = "Duration.Inf"
    def compare(other: Duration) = if (other eq this) 0 else 1
    def unary_- : Duration = MinusInf
  }

  /**
   * Infinite negative duration: lesser than any other and not equal to any other,
   * including itself.
   */
  val MinusInf: Duration = new Duration with Infinite {
    override def toString = "Duration.MinusInf"
    def compare(other: Duration) = if (other eq this) 0 else -1
    def unary_- : Duration = Inf
  }

  // Java Factories
  def create(length: Long, unit: TimeUnit): FiniteDuration = apply(length, unit)
  def create(length: Double, unit: TimeUnit): FiniteDuration = apply(length, unit)
  def create(length: Long, unit: String): FiniteDuration = apply(length, unit)
  def parse(s: String): Duration = unapply(s).get

  implicit object DurationIsOrdered extends Ordering[Duration] {
    def compare(a: Duration, b: Duration) = a compare b
  }
}

/**
 * Utility for working with java.util.concurrent.TimeUnit durations.
 *
 * <p/>
 * Examples:
 * <pre>
 * import scala.concurrent.util.Duration
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
 * import scala.concurrent.util.Duration._
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
abstract class Duration extends Serializable with Ordered[Duration] {
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

  def +(other: Duration): Duration
  def -(other: Duration): Duration
  def *(factor: Double): Duration
  def /(factor: Double): Duration
  def /(other: Duration): Double
  def unary_- : Duration
  def finite_? : Boolean
  def min(other: Duration): Duration = if (this < other) this else other
  def max(other: Duration): Duration = if (this > other) this else other
  def fromNow: Deadline = Deadline.now + this

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

object FiniteDuration {
  implicit object FiniteDurationIsOrdered extends Ordering[FiniteDuration] {
    def compare(a: FiniteDuration, b: FiniteDuration) = a compare b
  }

  def apply(length: Long, unit: TimeUnit) = 
    new FiniteDuration(length, unit)

  def apply(length: Long, unit: String) = {
    val (mult, timeUnit) = Duration.timeUnit(unit)
    new FiniteDuration(length * mult, timeUnit)
  }

}

class FiniteDuration(val length: Long, val unit: TimeUnit) extends Duration {
  import Duration._

  def toNanos = unit.toNanos(length)
  def toMicros = unit.toMicros(length)
  def toMillis = unit.toMillis(length)
  def toSeconds = unit.toSeconds(length)
  def toMinutes = unit.toMinutes(length)
  def toHours = unit.toHours(length)
  def toDays = unit.toDays(length)
  def toUnit(u: TimeUnit) = long2double(toNanos) / NANOSECONDS.convert(1, u)

  override def toString = this match {
    case Duration(1, SECONDS)      ⇒ "1 second"
    case Duration(x, SECONDS)      ⇒ x + " seconds"
    case Duration(1, MILLISECONDS) ⇒ "1 millisecond"
    case Duration(x, MILLISECONDS) ⇒ x + " milliseconds"
    case Duration(1, MICROSECONDS) ⇒ "1 microsecond"
    case Duration(x, MICROSECONDS) ⇒ x + " microseconds"
    case Duration(1, NANOSECONDS)  ⇒ "1 nanosecond"
    case Duration(x, NANOSECONDS)  ⇒ x + " nanoseconds"
  }

  def compare(other: Duration) =
    if (other.finite_?) {
      val me = toNanos
      val o = other.toNanos
      if (me > o) 1 else if (me < o) -1 else 0
    } else -other.compare(this)

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
  import DurationImplicits.Classifier

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

  def minutes = Duration(n * 60, SECONDS)
  def minute = Duration(n * 60, SECONDS)

  def hours = Duration(n * 3600, SECONDS)
  def hour =  Duration(n * 3600, SECONDS)

  def days = Duration(n * 86400, SECONDS)
  def day = Duration(n * 86400, SECONDS)

  def nanoseconds[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, NANOSECONDS))
  def nanos[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, NANOSECONDS))
  def nanosecond[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, NANOSECONDS))
  def nano[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, NANOSECONDS))

  def microseconds[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, MICROSECONDS))
  def micros[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, MICROSECONDS))
  def microsecond[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, MICROSECONDS))
  def micro[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, MICROSECONDS))

  def milliseconds[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, MILLISECONDS))
  def millis[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, MILLISECONDS))
  def millisecond[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, MILLISECONDS))
  def milli[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, MILLISECONDS))

  def seconds[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, SECONDS))
  def second[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, SECONDS))

  def minutes[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n * 60, SECONDS))
  def minute[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n * 60, SECONDS))

  def hours[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n * 3600, SECONDS))
  def hour[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n * 3600, SECONDS))

  def days[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n * 86400, SECONDS))
  def day[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n * 86400, SECONDS))
}

class DurationLong(n: Long) {
  import DurationImplicits.Classifier

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

  def minutes = Duration(n * 60, SECONDS)
  def minute = Duration(n * 60, SECONDS)

  def hours = Duration(n * 3600, SECONDS)
  def hour = Duration(n * 3600, SECONDS)

  def days = Duration(n * 86400, SECONDS)
  def day = Duration(n * 86400, SECONDS)

  def nanoseconds[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, NANOSECONDS))
  def nanos[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, NANOSECONDS))
  def nanosecond[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, NANOSECONDS))
  def nano[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, NANOSECONDS))

  def microseconds[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, MICROSECONDS))
  def micros[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, MICROSECONDS))
  def microsecond[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, MICROSECONDS))
  def micro[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, MICROSECONDS))

  def milliseconds[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, MILLISECONDS))
  def millis[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, MILLISECONDS))
  def millisecond[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, MILLISECONDS))
  def milli[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, MILLISECONDS))

  def seconds[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, SECONDS))
  def second[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n, SECONDS))

  def minutes[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n * 60, SECONDS))
  def minute[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n * 60, SECONDS))

  def hours[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n * 3600, SECONDS))
  def hour[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n * 3600, SECONDS))

  def days[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n * 86400, SECONDS))
  def day[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(n * 86400, SECONDS))
}

class DurationDouble(d: Double) {
  import DurationImplicits.Classifier

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

  def minutes = Duration(d * 60, SECONDS)
  def minute = Duration(d * 60, SECONDS)

  def hours = Duration(d * 3600, SECONDS)
  def hour = Duration(d * 3600, SECONDS)

  def days = Duration(d * 86400, SECONDS)
  def day = Duration(d * 86400, SECONDS)

  def nanoseconds[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d, NANOSECONDS))
  def nanos[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d, NANOSECONDS))
  def nanosecond[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d, NANOSECONDS))
  def nano[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d, NANOSECONDS))

  def microseconds[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d, MICROSECONDS))
  def micros[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d, MICROSECONDS))
  def microsecond[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d, MICROSECONDS))
  def micro[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d, MICROSECONDS))

  def milliseconds[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d, MILLISECONDS))
  def millis[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d, MILLISECONDS))
  def millisecond[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d, MILLISECONDS))
  def milli[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d, MILLISECONDS))

  def seconds[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d, SECONDS))
  def second[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d, SECONDS))

  def minutes[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d * 60, SECONDS))
  def minute[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d * 60, SECONDS))

  def hours[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d * 3600, SECONDS))
  def hour[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d * 3600, SECONDS))

  def days[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d * 86400, SECONDS))
  def day[C, CC <: Classifier[C]](c: C)(implicit ev: CC): CC#R = ev.convert(Duration(d * 86400, SECONDS))
}
