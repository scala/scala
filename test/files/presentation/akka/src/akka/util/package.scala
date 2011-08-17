/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.util

import java.util.concurrent.TimeUnit

package object duration {
  implicit def intToDurationInt(n: Int) = new DurationInt(n)
  implicit def longToDurationLong(n: Long) = new DurationLong(n)
  implicit def doubleToDurationDouble(d: Double) = new DurationDouble(d)

  implicit def pairIntToDuration(p: (Int, TimeUnit)) = Duration(p._1, p._2)
  implicit def pairLongToDuration(p: (Long, TimeUnit)) = Duration(p._1, p._2)
  implicit def durationToPair(d: Duration) = (d.length, d.unit)

  implicit def intMult(i: Int) = new {
    def *(d: Duration) = d * i
  }
  implicit def longMult(l: Long) = new {
    def *(d: Duration) = d * l
  }
  implicit def doubleMult(f: Double) = new {
    def *(d: Duration) = d * f
  }
}
