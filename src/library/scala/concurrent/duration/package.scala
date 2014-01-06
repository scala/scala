package scala.concurrent

import scala.language.implicitConversions

package object duration {
  /**
   * This object can be used as closing token if you prefer dot-less style but do not want
   * to enable language.postfixOps:
   *
   * {{{
   * import scala.concurrent.duration._
   *
   * val duration = 2 seconds span
   * }}}
   */
  object span

  /**
   * This object can be used as closing token for declaring a deadline at some future point
   * in time:
   *
   * {{{
   * import scala.concurrent.duration._
   *
   * val deadline = 3 seconds fromNow
   * }}}
   */
  object fromNow

  type TimeUnit          = java.util.concurrent.TimeUnit
  final val DAYS         = java.util.concurrent.TimeUnit.DAYS
  final val HOURS        = java.util.concurrent.TimeUnit.HOURS
  final val MICROSECONDS = java.util.concurrent.TimeUnit.MICROSECONDS
  final val MILLISECONDS = java.util.concurrent.TimeUnit.MILLISECONDS
  final val MINUTES      = java.util.concurrent.TimeUnit.MINUTES
  final val NANOSECONDS  = java.util.concurrent.TimeUnit.NANOSECONDS
  final val SECONDS      = java.util.concurrent.TimeUnit.SECONDS

  implicit def pairIntToDuration(p: (Int, TimeUnit)): Duration         = Duration(p._1.toLong, p._2)
  implicit def pairLongToDuration(p: (Long, TimeUnit)): FiniteDuration = Duration(p._1, p._2)
  implicit def durationToPair(d: Duration): (Long, TimeUnit)           = (d.length, d.unit)

  implicit final class DurationInt(private val n: Int) extends AnyVal with DurationConversions {
    override protected def durationIn(unit: TimeUnit): FiniteDuration = Duration(n.toLong, unit)
  }

  implicit final class DurationLong(private val n: Long) extends AnyVal with DurationConversions {
    override protected def durationIn(unit: TimeUnit): FiniteDuration = Duration(n, unit)
  }

  implicit final class DurationDouble(private val d: Double) extends AnyVal with DurationConversions {
    override protected def durationIn(unit: TimeUnit): FiniteDuration =
      Duration(d, unit) match {
        case f: FiniteDuration => f
        case _ => throw new IllegalArgumentException("Duration DSL not applicable to " + d)
      }
  }

  /*
   * Avoid reflection based invocation by using non-duck type
   */
  implicit final class IntMult(private val i: Int) extends AnyVal {
    def *(d: Duration) = d * i.toDouble
    def *(d: FiniteDuration) = d * i.toLong
  }

  implicit final class LongMult(private val i: Long) extends AnyVal {
    def *(d: Duration) = d * i.toDouble
    def *(d: FiniteDuration) = d * i.toLong
  }

  implicit final class DoubleMult(private val f: Double) extends AnyVal {
    def *(d: Duration) = d * f.toDouble
  }
}
