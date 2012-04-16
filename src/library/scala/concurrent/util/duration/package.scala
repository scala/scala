package scala.concurrent.util

import java.util.concurrent.TimeUnit
import language.implicitConversions

package object duration {

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

  implicit def intMult(i: Int) = new IntMult(i)
  implicit def longMult(l: Long) = new LongMult(l)
  implicit def doubleMult(f: Double) = new DoubleMult(f)
}
