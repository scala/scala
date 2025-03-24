/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.concurrent.duration

import DurationConversions._

// Would be nice to limit the visibility of this trait a little bit,
// but it crashes scalac to do so.
trait DurationConversions extends Any {
  protected def durationIn(unit: TimeUnit): FiniteDuration

  def nanoseconds: FiniteDuration  = durationIn(NANOSECONDS)
  def nanos: FiniteDuration        = nanoseconds
  def nanosecond: FiniteDuration   = nanoseconds
  def nano: FiniteDuration         = nanoseconds

  def microseconds: FiniteDuration = durationIn(MICROSECONDS)
  def micros: FiniteDuration       = microseconds
  def microsecond: FiniteDuration  = microseconds
  def micro: FiniteDuration        = microseconds

  def milliseconds: FiniteDuration = durationIn(MILLISECONDS)
  def millis: FiniteDuration       = milliseconds
  def millisecond: FiniteDuration  = milliseconds
  def milli: FiniteDuration        = milliseconds

  def seconds: FiniteDuration      = durationIn(SECONDS)
  def second: FiniteDuration       = seconds

  def minutes: FiniteDuration      = durationIn(MINUTES)
  def minute: FiniteDuration       = minutes

  def hours: FiniteDuration        = durationIn(HOURS)
  def hour: FiniteDuration         = hours

  def days: FiniteDuration         = durationIn(DAYS)
  def day: FiniteDuration          = days

  def nanoseconds[C](c: C)(implicit ev: Classifier[C]): ev.R  = ev.convert(nanoseconds)
  def nanos[C](c: C)(implicit ev: Classifier[C]): ev.R        = nanoseconds(c)
  def nanosecond[C](c: C)(implicit ev: Classifier[C]): ev.R   = nanoseconds(c)
  def nano[C](c: C)(implicit ev: Classifier[C]): ev.R         = nanoseconds(c)

  def microseconds[C](c: C)(implicit ev: Classifier[C]): ev.R = ev.convert(microseconds)
  def micros[C](c: C)(implicit ev: Classifier[C]): ev.R       = microseconds(c)
  def microsecond[C](c: C)(implicit ev: Classifier[C]): ev.R  = microseconds(c)
  def micro[C](c: C)(implicit ev: Classifier[C]): ev.R        = microseconds(c)

  def milliseconds[C](c: C)(implicit ev: Classifier[C]): ev.R = ev.convert(milliseconds)
  def millis[C](c: C)(implicit ev: Classifier[C]): ev.R       = milliseconds(c)
  def millisecond[C](c: C)(implicit ev: Classifier[C]): ev.R  = milliseconds(c)
  def milli[C](c: C)(implicit ev: Classifier[C]): ev.R        = milliseconds(c)

  def seconds[C](c: C)(implicit ev: Classifier[C]): ev.R      = ev.convert(seconds)
  def second[C](c: C)(implicit ev: Classifier[C]): ev.R       = seconds(c)

  def minutes[C](c: C)(implicit ev: Classifier[C]): ev.R      = ev.convert(minutes)
  def minute[C](c: C)(implicit ev: Classifier[C]): ev.R       = minutes(c)

  def hours[C](c: C)(implicit ev: Classifier[C]): ev.R        = ev.convert(hours)
  def hour[C](c: C)(implicit ev: Classifier[C]): ev.R         = hours(c)

  def days[C](c: C)(implicit ev: Classifier[C]): ev.R         = ev.convert(days)
  def day[C](c: C)(implicit ev: Classifier[C]): ev.R          = days(c)
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
    def convert(d: FiniteDuration): FiniteDuration = d
  }

  implicit object fromNowConvert extends Classifier[fromNow.type] {
    type R = Deadline
    def convert(d: FiniteDuration): Deadline = Deadline.now + d
  }

}
