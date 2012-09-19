/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2012, LAMP/EPFL             **
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
