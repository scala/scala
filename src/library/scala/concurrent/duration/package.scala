package scala.concurrent

import scala.language.implicitConversions

package object duration {
  // FIXME - these need documenting.
  object span
  object fromNow

  type TimeUnit          = java.util.concurrent.TimeUnit
  final val DAYS         = java.util.concurrent.TimeUnit.DAYS
  final val HOURS        = java.util.concurrent.TimeUnit.HOURS
  final val MICROSECONDS = java.util.concurrent.TimeUnit.MICROSECONDS
  final val MILLISECONDS = java.util.concurrent.TimeUnit.MILLISECONDS
  final val MINUTES      = java.util.concurrent.TimeUnit.MINUTES
  final val NANOSECONDS  = java.util.concurrent.TimeUnit.NANOSECONDS
  final val SECONDS      = java.util.concurrent.TimeUnit.SECONDS

  implicit def pairIntToDuration(p: (Int, TimeUnit)): Duration         = Duration(p._1, p._2)
  implicit def pairLongToDuration(p: (Long, TimeUnit)): FiniteDuration = Duration(p._1, p._2)
  implicit def durationToPair(d: Duration): (Long, TimeUnit)           = (d.length, d.unit)

  implicit final class DurationInt(val n: Int) extends AnyVal with DurationConversions {
    override protected def durationIn(unit: TimeUnit): FiniteDuration = Duration(n, unit)
  }

  implicit final class DurationLong(val n: Long) extends AnyVal with DurationConversions {
    override protected def durationIn(unit: TimeUnit): FiniteDuration = Duration(n, unit)
  }

  implicit final class DurationDouble(val d: Double) extends AnyVal with DurationConversions {
    override protected def durationIn(unit: TimeUnit): FiniteDuration =
      Duration(d, unit) match {
        case f: FiniteDuration => f
        case _ => throw new IllegalArgumentException("Duration DSL not applicable to " + d)
      }
  }

  /*
   * Avoid reflection based invocation by using non-duck type
   */
  implicit final class IntMult(val i: Int) extends AnyVal {
    def *(d: Duration) = d * i
  }

  implicit final class LongMult(val i: Long) extends AnyVal {
    def *(d: Duration) = d * i
  }

  implicit final class DoubleMult(val f: Double) extends AnyVal {
    def *(d: Duration) = d * f
  }
}
