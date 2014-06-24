/**
 *  Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

import scala.concurrent.duration._
import scala.reflect._
import scala.tools.partest.TestUtil.intercept

import scala.language.{ postfixOps }

object Test extends App {

  implicit class Assert(val left: Any) extends AnyVal {
    import Duration.Undefined
    def mustBe(right: Any) = right match {
      case r: Double if r.isNaN                        => assert(left.asInstanceOf[Double].isNaN, s"$left was not NaN")
      case r: Double if r == 0 && r.compareTo(0) == -1 => assert(left == 0 && left.asInstanceOf[Double].compareTo(0) == -1, s"$left was not -0.0")
      case Undefined                                   => assert(left.asInstanceOf[AnyRef] eq Undefined, s"$left was not Undefined")
      case _                                           => assert(left == right, s"$left was not equal to $right")
    }
  }

  val zero = 0 seconds
  val one = 1 second
  val two = one + one
  val three = 3 * one
  val inf = Duration.Inf
  val minf = Duration.MinusInf
  val undef = Duration.Undefined
  val inputs = List(zero, one, inf, minf, undef)
  val nan = Double.NaN

  // test field ops
  one.isFinite mustBe true
  0 * one mustBe zero
  2 * one mustBe two
  three - two mustBe one
  three / 3 mustBe one
  two / one mustBe 2
  one + zero mustBe one
  one / 1000000 mustBe 1.micro


  // test infinities

   inf.isFinite mustBe false
  minf.isFinite mustBe false

    inf mustBe  inf
   minf mustBe minf
   -inf mustBe minf
  -minf mustBe  inf

  minf +  inf mustBe undef
   inf -  inf mustBe undef
   inf + minf mustBe undef
  minf - minf mustBe undef

   inf +  inf mustBe  inf
   inf - minf mustBe  inf
  minf -  inf mustBe minf
  minf + minf mustBe minf

  for (i <- Seq(zero, one, two, three)) {
    i - inf mustBe minf
    i - minf mustBe inf
  }

  inf.compareTo(inf) mustBe 0
  inf.compareTo(one) mustBe 1
  inf.compareTo(minf) mustBe 1
  minf.compareTo(minf) mustBe 0
  minf.compareTo(one) mustBe -1
  minf.compareTo(inf) mustBe -1

  assert(inf != minf)
  assert(minf != inf)
  assert(one != inf)
  assert(minf != one)

  inf mustBe (minf * -1d)
  inf mustBe (minf / -1d)

   one /  inf mustBe  0d
  -one /  inf mustBe -0d
   one / minf mustBe -0d
  -one / minf mustBe  0d

  inputs filterNot (_.isFinite) foreach (x => x / zero mustBe x.toUnit(DAYS))
  inputs filterNot (_.isFinite) foreach (_ * 0d mustBe undef)
  inputs filterNot (_.isFinite) foreach (_ * -0d mustBe undef)
  inputs filterNot (_.isFinite) foreach (x => x * Double.PositiveInfinity mustBe x)
  inputs filterNot (_.isFinite) foreach (x => x * Double.NegativeInfinity mustBe -x)

  inf.toUnit(SECONDS) mustBe Double.PositiveInfinity
  minf.toUnit(MINUTES) mustBe Double.NegativeInfinity
  Duration.fromNanos(Double.PositiveInfinity) mustBe inf
  Duration.fromNanos(Double.NegativeInfinity) mustBe minf


  // test undefined & NaN

  undef.isFinite mustBe false
  -undef mustBe undef
  assert(undef != undef)
  assert(undef eq undef)

  inputs foreach (_ + undef mustBe undef)
  inputs foreach (_ - undef mustBe undef)
  inputs foreach (_ / undef mustBe nan)
  inputs foreach (_ / nan mustBe undef)
  inputs foreach (_ * nan mustBe undef)
  inputs foreach (undef + _ mustBe undef)
  inputs foreach (undef - _ mustBe undef)
  inputs foreach (undef / _ mustBe nan)
  undef / 1 mustBe undef
  undef / nan mustBe undef
  undef * 1 mustBe undef
  undef * nan mustBe undef
  inputs foreach (x => x / zero mustBe x.toUnit(SECONDS) / 0d)
  inputs foreach (x => x / 0d mustBe Duration.fromNanos(x.toUnit(NANOSECONDS) / 0d))
  inputs foreach (x => x / -0d mustBe Duration.fromNanos(x.toUnit(NANOSECONDS) / -0d))

  inputs filterNot (_ eq undef) foreach (_ compareTo undef mustBe -1)
  inputs filterNot (_ eq undef) foreach (undef compareTo _ mustBe 1)
  undef compare undef mustBe 0

  undef.toUnit(DAYS) mustBe nan
  Duration.fromNanos(nan) mustBe undef


  // test overflow protection
  for (unit ← Seq(DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS, MICROSECONDS, NANOSECONDS)) {
    val x = unit.convert(Long.MaxValue, NANOSECONDS)
    val dur = Duration(x, unit)
    val mdur = Duration(-x, unit)
    -mdur mustBe (dur)
    intercept[IllegalArgumentException] { Duration(x + 10000000d, unit) }
    intercept[IllegalArgumentException] { Duration(-x - 10000000d, unit) }
    if (unit != NANOSECONDS) {
      intercept[IllegalArgumentException] { Duration(x + 1, unit) }
      intercept[IllegalArgumentException] { Duration(-x - 1, unit) }
    }
    intercept[IllegalArgumentException] { dur + 1.day }
    intercept[IllegalArgumentException] { mdur - 1.day }
    intercept[IllegalArgumentException] { dur * 1.1 }
    intercept[IllegalArgumentException] { mdur * 1.1 }
    intercept[IllegalArgumentException] { dur * 2.1 }
    intercept[IllegalArgumentException] { mdur * 2.1 }
    intercept[IllegalArgumentException] { dur / 0.9 }
    intercept[IllegalArgumentException] { mdur / 0.9 }
    intercept[IllegalArgumentException] { dur / 0.4 }
    intercept[IllegalArgumentException] { mdur / 0.4 }
    Duration(x + unit.toString.toLowerCase)
    Duration("-" + x + unit.toString.toLowerCase)
    intercept[IllegalArgumentException] { Duration("%.0f".format(x + 10000000d) + unit.toString.toLowerCase) }
    intercept[IllegalArgumentException] { Duration("-%.0f".format(x + 10000000d) + unit.toString.toLowerCase) }
  }
  intercept[IllegalArgumentException] { Duration.fromNanos(1e20) }
  intercept[IllegalArgumentException] { Duration.fromNanos(-1e20) }


  // test precision
  1.second + 1.millisecond mustBe 1001.milliseconds
  100000.days + 1.nanosecond mustBe 8640000000000000001L.nanoseconds
  1.5.seconds.toSeconds mustBe 1
  (-1.5).seconds.toSeconds mustBe -1


  // test unit stability
  1000.millis.unit mustBe MILLISECONDS
  (1000.millis + 0.days).unit mustBe MILLISECONDS
  1.second.unit mustBe SECONDS
  (1.second + 1.millisecond).unit mustBe MILLISECONDS


  // test Deadline
  val dead = 2.seconds.fromNow
  val dead2 = 2 seconds fromNow

  { val l = dead.timeLeft; assert(l > 1.second, s"$l <= 1.second") }
  { val l = dead2.timeLeft; assert(l > 1.second, s"$l <= 1.second") }

  Thread.sleep(1.second.toMillis)

  // unfortunately it can happen that the sleep() returns early without throwing
  { val l = dead.timeLeft; assert(l <= 1100.millis, s"$l > 1100.millis") }
  { val l = dead2.timeLeft; assert(l <= 1100.millis, s"$l > 1100.millis") }


  // test integer mul/div
  500.millis * 2 mustBe 1.second
  (500.millis * 2).unit mustBe MILLISECONDS
  1.second / 2 mustBe 500.millis
  (1.second / 2).unit mustBe MILLISECONDS


  // check statically retaining finite-ness
  val finiteDuration: FiniteDuration = 1.second * 2 / 3 mul 5 div 4 plus 3.seconds minus 1.millisecond min 1.second max 1.second
  val finite2: FiniteDuration = 2 * 1.second + 3L * 2.seconds
  finite2 mustBe 8.seconds
  ((2 seconds fromNow).timeLeft: FiniteDuration) < 4.seconds mustBe true
  val finite3: FiniteDuration = 3.5 seconds span

}
