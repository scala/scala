/**
 * Copyright (C) 2009-2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.concurrent.util

import java.util.concurrent.TimeUnit

object duration {
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
