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
import java.lang.{ Double => JDouble }
import language.implicitConversions

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

// TODO: "Inf", "PlusInf", "MinusInf", where did these names come from?
// TODO: Duration.create(n, DAYS) == Duration(Long.MaxValue, NANOSECONDS) forall (n: Double) >= 106752d
object Duration {
  implicit def timeLeft(implicit d: Deadline): Duration = d.timeLeft

  def apply(length: Double, unit: TimeUnit): FiniteDuration = fromNanos(unit.toNanos(1) * length)
  def apply(length: Long, unit: TimeUnit): FiniteDuration   = new FiniteDuration(length, unit)
  def apply(length: Long, unit: String): FiniteDuration     = new FiniteDuration(length,  Duration.timeUnit(unit))

  /**
   * Parse String into Duration.  Format is `"<length><unit>"`, where
   * whitespace is allowed before, between and after the parts. Infinities are
   * designated by `"Inf"`, `"PlusInf"`, `"+Inf"` and `"-Inf"` or `"MinusInf"`.
   * Throws exception if format is not parseable.
   */
  def apply(s: String): Duration = {
    val s1: String = s filterNot (_.isWhitespace)
    s1 match {
      case "Inf" | "PlusInf" | "+Inf" => Inf
      case "MinusInf" | "-Inf"        => MinusInf
      case _                          =>
        val unitName = s1.reverse takeWhile (_.isLetter) reverse;
        def length   = JDouble.parseDouble(s1 dropRight unitName.length)
        timeUnit get unitName match {
          case Some(unit) => Duration(length, unit)
          case _          => sys.error("format error " + s)
        }
    }
  }

  // "ms milli millisecond" -> List("ms", "milli", "millis", "millisecond", "milliseconds")
  private def words(s: String) = (s.trim split "\\s+").toList
  private def expandLabels(labels: String): List[String] = {
    val hd :: rest = words(labels)
    hd :: rest.flatMap(s => List(s, s + "s"))
  }
  private val timeUnitLabels = List(
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

  def unapply(s: String): Option[(Long, TimeUnit)] =
    ( try Some(apply(s)) catch { case _: RuntimeException => None } ) flatMap unapply

  def unapply(d: Duration): Option[(Long, TimeUnit)] =
    if (d.isFinite) Some((d.length, d.unit)) else None

  def fromNanos(nanos: Double): FiniteDuration =
    fromNanos((nanos + 0.5).toLong)

  def fromNanos(nanos: Long): FiniteDuration = {
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

  /**
   * Parse TimeUnit from string representation.
   */

  val Zero: FiniteDuration = new FiniteDuration(0, NANOSECONDS)

  object Undefined extends Infinite {
    private def fail(what: String) = throw new IllegalArgumentException(s"cannot $what Undefined duration")

    override def toString = "Duration.Undefined"
    override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]
    override def +(other: Duration): Duration = fail("add")
    override def -(other: Duration): Duration = fail("subtract")
    override def *(factor: Double): Duration  = fail("multiply")
    override def /(factor: Double): Duration  = fail("divide")
    override def /(other: Duration): Double   = fail("divide")
    def compare(other: Duration) = fail("compare")
    def unary_- : Duration = fail("negate")
  }

  sealed abstract class Infinite extends Duration {
    def +(other: Duration): Duration = other match {
      case x: Infinite if x ne this => throw new IllegalArgumentException("illegal addition of infinities")
      case _                        => this
    }
    // Is this really intended to throw if the argument is "this" but otherwise return this?
    def -(other: Duration): Duration =
      if (other ne this) this
      else throw new IllegalArgumentException("illegal subtraction of infinities")

    def *(factor: Double): Duration = this
    def /(factor: Double): Duration = this
    def /(other: Duration): Double = other match {
      case _: Infinite => throw new IllegalArgumentException("illegal division of infinities")
      // maybe questionable but pragmatic: Inf / 0 => Inf
      case x           => Double.PositiveInfinity * (if ((this > Zero) ^ (other >= Zero)) -1 else 1)
    }

    final def isFinite() = false

    private def fail(what: String) = throw new IllegalArgumentException(s"$what not allowed on infinite Durations")
    def length: Long    = fail("length")
    def toNanos: Long   = fail("toNanos")
    def toMicros: Long  = fail("toMicros")
    def toMillis: Long  = fail("toMillis")
    def toSeconds: Long = fail("toSeconds")
    def toMinutes: Long = fail("toMinutes")
    def toHours: Long   = fail("toHours")
    def toDays: Long    = fail("toDays")

    def unit: TimeUnit                 = fail("unit")
    def toUnit(unit: TimeUnit): Double = fail("toUnit")
  }

  /**
   * Infinite duration: greater than any other and not equal to any other,
   * including itself.
   */
  val Inf: Infinite = new Infinite {
    override def toString = "Duration.Inf"
    def compare(other: Duration) = if (other eq this) 0 else 1
    def unary_- : Duration = MinusInf
  }

  /**
   * Infinite negative duration: lesser than any other and not equal to any other,
   * including itself.
   */
  val MinusInf: Infinite = new Infinite {
    override def toString = "Duration.MinusInf"
    def compare(other: Duration) = if (other eq this) 0 else -1
    def unary_- : Duration = Inf
  }

  // Java Factories
  def create(length: Long, unit: TimeUnit): FiniteDuration   = apply(length, unit)
  def create(length: Double, unit: TimeUnit): FiniteDuration = apply(length, unit)
  def create(length: Long, unit: String): FiniteDuration     = apply(length, unit)
  def create(s: String): Duration                            = apply(s)

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
  def isFinite(): Boolean
  def min(other: Duration): Duration = if (this < other) this else other
  def max(other: Duration): Duration = if (this > other) this else other
  def fromNow: Deadline = Deadline.now + this

  // Java API
  def div(factor: Double)    = this / factor
  def div(other: Duration)   = this / other
  def gt(other: Duration)    = this > other
  def gteq(other: Duration)  = this >= other
  def lt(other: Duration)    = this < other
  def lteq(other: Duration)  = this <= other
  def minus(other: Duration) = this - other
  def mul(factor: Double)    = this * factor
  def neg()                  = -this
  def plus(other: Duration)  = this + other
}

object FiniteDuration {
  implicit object FiniteDurationIsOrdered extends Ordering[FiniteDuration] {
    def compare(a: FiniteDuration, b: FiniteDuration) = a compare b
  }

  def apply(length: Long, unit: TimeUnit) = new FiniteDuration(length, unit)
  def apply(length: Long, unit: String)   = new FiniteDuration(length, Duration.timeUnit(unit))
}

class FiniteDuration(val length: Long, val unit: TimeUnit) extends Duration {
  import Duration._

  def toNanos   = unit.toNanos(length)
  def toMicros  = unit.toMicros(length)
  def toMillis  = unit.toMillis(length)
  def toSeconds = unit.toSeconds(length)
  def toMinutes = unit.toMinutes(length)
  def toHours   = unit.toHours(length)
  def toDays    = unit.toDays(length)
  def toUnit(u: TimeUnit) = toNanos.toDouble / NANOSECONDS.convert(1, u)

  private def unitString = timeUnitName(unit) + ( if (length == 1) "" else "s" )
  override def toString = "" + length + " " + unitString

  def compare(other: Duration) = other match {
    case x: FiniteDuration => toNanos compare x.toNanos
    case _                 => -(other compare this)
  }
  def +(other: Duration) = other match {
    case x: FiniteDuration => fromNanos(toNanos + x.toNanos)
    case _                 => other
  }
  def -(other: Duration) = other match {
    case x: FiniteDuration => fromNanos(toNanos - x.toNanos)
    case _                 => other
  }

  def *(factor: Double) = fromNanos(toNanos.toDouble * factor)

  def /(factor: Double) = fromNanos(toNanos.toDouble / factor)

  def /(other: Duration) = if (other.isFinite) toNanos.toDouble / other.toNanos else 0

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
  override protected def durationIn(unit: TimeUnit): FiniteDuration = Duration(d, unit)
}
