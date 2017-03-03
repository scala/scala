import scala.concurrent.duration._
import scala.language.postfixOps

object Test extends App {
  List(
    (60 minutes,    1 hour),
    (2000 millis,   2 seconds),
    (2000 micros,   2 millis),
    (2000 nanos,    2 micros),
    (2000000 nanos, 2 millis),
    (48 hours,      2 days),
    (5 seconds,     5 seconds),
    (1 second,      1 second)
  ) foreach {
    case (x, expected) =>
      val actual = x.toCoarsest
      assert(actual.unit == expected.unit, s"$actual, $expected")
      assert(actual.length == expected.length, s"$actual, $expected")
  }

  List(
    45 minutes,
    500 millis,
    1500 millis,
    23 hours,
    40 days
  ) foreach (x => assert(x == x.toCoarsest, x))

  // toCoarsest on a FiniteDuration should return a FiniteDuration
  val finite: FiniteDuration = 1.second.toCoarsest
}
