/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.duration

import DurationConversions._

// Would be nice to limit the visibility of this trait a little bit,
// but it crashes scalac to do so.
trait DurationConversions extends Any {
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

  def nanoseconds[C](c: C)(implicit ev: Classifier[C]): ev.R = ev.convert(nanoseconds)
  def nanos[C](c: C)(implicit ev: Classifier[C]): ev.R = nanoseconds(c)
  def nanosecond[C](c: C)(implicit ev: Classifier[C]): ev.R = nanoseconds(c)
  def nano[C](c: C)(implicit ev: Classifier[C]): ev.R = nanoseconds(c)

  def microseconds[C](c: C)(implicit ev: Classifier[C]): ev.R = ev.convert(microseconds)
  def micros[C](c: C)(implicit ev: Classifier[C]): ev.R = microseconds(c)
  def microsecond[C](c: C)(implicit ev: Classifier[C]): ev.R = microseconds(c)
  def micro[C](c: C)(implicit ev: Classifier[C]): ev.R = microseconds(c)

  def milliseconds[C](c: C)(implicit ev: Classifier[C]): ev.R = ev.convert(milliseconds)
  def millis[C](c: C)(implicit ev: Classifier[C]): ev.R = milliseconds(c)
  def millisecond[C](c: C)(implicit ev: Classifier[C]): ev.R = milliseconds(c)
  def milli[C](c: C)(implicit ev: Classifier[C]): ev.R = milliseconds(c)

  def seconds[C](c: C)(implicit ev: Classifier[C]): ev.R = ev.convert(seconds)
  def second[C](c: C)(implicit ev: Classifier[C]): ev.R = seconds(c)

  def minutes[C](c: C)(implicit ev: Classifier[C]): ev.R = ev.convert(minutes)
  def minute[C](c: C)(implicit ev: Classifier[C]): ev.R = minutes(c)

  def hours[C](c: C)(implicit ev: Classifier[C]): ev.R = ev.convert(hours)
  def hour[C](c: C)(implicit ev: Classifier[C]): ev.R = hours(c)

  def days[C](c: C)(implicit ev: Classifier[C]): ev.R = ev.convert(days)
  def day[C](c: C)(implicit ev: Classifier[C]): ev.R = days(c)
}

/**
 * This object just holds some cogs which make the DSL machine work, not for direct consumption.
 */
object DurationConversions {
  trait Classifier[C] {
    type R
    def convert(d: FiniteDuration): R
  }

  implicit object spanConvert extends Classifier[span.type] {
    type R = FiniteDuration
    def convert(d: FiniteDuration) = d
  }

  implicit object fromNowConvert extends Classifier[fromNow.type] {
    type R = Deadline
    def convert(d: FiniteDuration) = Deadline.now + d
  }

}
