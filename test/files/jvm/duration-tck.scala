/**
 *  Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
  
import scala.concurrent.util._
import duration._
import scala.reflect._
import java.util.concurrent.TimeUnit._

object Test extends App {

  implicit class Assert(val left: Any) extends AnyVal {
    def =!=(right: Any) = assert(left == right, s"$left was not equal to $right")
  }

  def intercept[T <: Exception : ClassTag](code: => Unit) =
    try { code; assert(false, "did not throw expected exception " + classTag[T]) }
    catch {
      case ex: Exception => if (classTag[T].runtimeClass isAssignableFrom ex.getClass) () else throw ex
    }

  { // test field ops
    val zero = 0 seconds
    val one = 1 second
    val two = one + one
    val three = 3 * one
    (0 * one) =!= (zero)
    (2 * one) =!= (two)
    (three - two) =!= (one)
    (three / 3) =!= (one)
    (two / one) =!= (2)
    (one + zero) =!= (one)
    (one / 1000000) =!= (1.micro)
  }

  { // test infinities
    val one = 1.second
    val inf = Duration.Inf
    val minf = Duration.MinusInf
    (-inf) =!= (minf)
    intercept[IllegalArgumentException] { minf + inf }
    intercept[IllegalArgumentException] { inf - inf }
    intercept[IllegalArgumentException] { inf + minf }
    intercept[IllegalArgumentException] { minf - minf }
    (inf + inf) =!= (inf)
    (inf - minf) =!= (inf)
    (minf - inf) =!= (minf)
    (minf + minf) =!= (minf)
    assert(inf == inf)
    assert(minf == minf)
    inf.compareTo(inf) =!= (0)
    inf.compareTo(one) =!= (1)
    minf.compareTo(minf) =!= (0)
    minf.compareTo(one) =!= (-1)
    assert(inf != minf)
    assert(minf != inf)
    assert(one != inf)
    assert(minf != one)
    inf =!= (minf * -1d)
    inf =!= (minf / -1d)
  }

  { // test overflow protection
    for (unit ← Seq(DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS, MICROSECONDS, NANOSECONDS)) {
      val x = unit.convert(Long.MaxValue, NANOSECONDS)
      val dur = Duration(x, unit)
      val mdur = Duration(-x, unit)
      -mdur =!= (dur)
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
  }

  { // test Deadline
    val dead = 2.seconds.fromNow
    val dead2 = 2 seconds fromNow
    // view bounds vs. very local type inference vs. operator precedence: sigh
    assert(dead.timeLeft > (1 second: Duration))
    assert(dead2.timeLeft > (1 second: Duration))
    Thread.sleep(1.second.toMillis)
    assert(dead.timeLeft < (1 second: Duration))
    assert(dead2.timeLeft < (1 second: Duration))
  }

  { // check statically retaining finite-ness
    val d: FiniteDuration = 1.second * 2 / 1.4 mul 1.1 div 2.1 plus 3.seconds minus 1.millisecond min 1.second max 1.second
  }

}
