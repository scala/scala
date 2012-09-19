import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._
import math._
import concurrent.duration.Duration.fromNanos

object Test extends Properties("Division of Duration by Long") {

  val weightedLong =
    frequency(
      1 -> choose(-128L, 127L),
      1 -> (arbitrary[Byte] map (_.toLong << 8)),
      1 -> (arbitrary[Byte] map (_.toLong << 16)),
      1 -> (arbitrary[Byte] map (_.toLong << 24)),
      1 -> (arbitrary[Byte] map (_.toLong << 32)),
      1 -> (arbitrary[Byte] map (_.toLong << 40)),
      1 -> (arbitrary[Byte] map (_.toLong << 48)),
      1 -> (choose(-127L, 127L) map (_ << 56))
    )

  val genTwoSmall = for {
    a <- weightedLong
    b <- choose(-(Long.MaxValue / max(1, abs(a))), Long.MaxValue / max(1, abs(a)))
  } yield (a, b)

  val genTwoLarge = for {
    a <- weightedLong
    b <- arbitrary[Long] suchThat (b => (abs(b) > Long.MaxValue / max(1, abs(a))))
  } yield (a, b)

  val genClose = for {
    a <- weightedLong
    if a != 0
    b <- choose(Long.MaxValue / a - 10, Long.MaxValue / a + 10)
  } yield (a, b)

  val genBorderline =
    frequency(
      1 -> (Long.MinValue, 0L),
      1 -> (Long.MinValue, 1L),
      1 -> (Long.MinValue, -1L),
      1 -> (0L, Long.MinValue),
      1 -> (1L, Long.MinValue),
      1 -> (-1L, Long.MinValue),
      90 -> genClose
    )

  def mul(a: Long, b: Long): Long = {
    (fromNanos(a) * b).toNanos
  }

  property("without overflow") = forAll(genTwoSmall) { case (a, b) =>
    a * b == mul(a, b)
  }

  property("with overflow") = forAll(genTwoLarge) { case (a, b) =>
    try { mul(a, b); false } catch { case _: IllegalArgumentException => true }
  }

  property("on overflow edge cases") = forAll(genBorderline) { case (a, b) =>
    val shouldFit =
      a != Long.MinValue && // must fail due to illegal duration length
      (b != Long.MinValue || a == 0) && // Long factor may only be MinValue if the duration is zero, otherwise the result will be illegal
      (abs(b) <= Long.MaxValue / max(1, abs(a))) // check the rest against the “safe” division method
    try { mul(a, b); shouldFit }
    catch { case _: IllegalArgumentException => !shouldFit }
  }
}
