package scala.concurrent

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

  implicit def pairIntToDuration(p: (Int, TimeUnit)): Duration         = Duration(p._1, p._2)
  implicit def pairLongToDuration(p: (Long, TimeUnit)): FiniteDuration = Duration(p._1, p._2)
  implicit def durationToPair(d: Duration): (Long, TimeUnit)           = (d.length, d.unit)

  final class DurationInt(val n: Int) extends DurationConversions {
    override protected def durationIn(unit: TimeUnit): FiniteDuration = Duration(n, unit)
  }
  implicit def intToDurationInt(n: Int) = new DurationInt(n)

  final class DurationLong(val n: Long) extends DurationConversions {
    override protected def durationIn(unit: TimeUnit): FiniteDuration = Duration(n, unit)
  }
  implicit def longToDurationLong(n: Long) = new DurationLong(n)

  final class DurationDouble(val d: Double) extends DurationConversions {
    override protected def durationIn(unit: TimeUnit): FiniteDuration =
      Duration(d, unit) match {
        case f: FiniteDuration => f
        case _ => throw new IllegalArgumentException("Duration DSL not applicable to " + d)
      }
  }
  implicit def doubleToDurationDouble(d: Double) = new DurationDouble(d)

  /*
   * Avoid reflection based invocation by using non-duck type
   */
  final class IntMult(val i: Int) {
    def *(d: Duration) = d * i
    def *(d: FiniteDuration) = d * i
  }
  implicit def intToIntMult(i: Int) = new IntMult(i)

  final class LongMult(val i: Long) {
    def *(d: Duration) = d * i
    def *(d: FiniteDuration) = d * i
  }
  implicit def longToLongMult(i: Long) = new LongMult(i)

  final class DoubleMult(val f: Double) {
    def *(d: Duration) = d * f
  }
  implicit def doubleToDoubleMult(f: Double) = new DoubleMult(f)
}
