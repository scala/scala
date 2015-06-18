package scala.math

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import java.math.{BigDecimal => BD, MathContext => MC}

/* Tests various maps by making sure they all agree on the same answers. */
@RunWith(classOf[JUnit4])
class BigDecimalTest {
  
  // Motivated by SI-6173: BigDecimal#isWhole implementation is very heap intensive
  @Test
  def isWholeTest() {
    val wholes = List(
      BigDecimal(1),
      BigDecimal(10L),
      BigDecimal(14.000),
      BigDecimal(new BD("19127981892347012385719827340123471923850195")),
      BigDecimal("1e1000000000"),
      BigDecimal(14.1928857191985e22),
      BigDecimal(14.12519823759817, new MC(2))
    )
    val fracs = List(
      BigDecimal(0.1),
      BigDecimal(new BD("1.000000000000000000000000000000000001")),
      BigDecimal(new BD("275712375971892375127591745810580123751.99999")),
      BigDecimal("14.19238571927581e6"),
      BigDecimal("912834718237510238591285")/2
    )
    assert(wholes.forall(_.isWhole) && fracs.forall(! _.isWhole))
  }

  // Motivated by SI-6699: BigDecimal.isValidDouble behaves unexpectedly
  @Test
  def isValidDoubleTest() {
    val valids = List(
      BigDecimal(1),
      BigDecimal(19571.125),
      BigDecimal.decimal(0.1),
      BigDecimal(1e15)
    )
    val invalids = List(
      BigDecimal(new BD("1.0000000000000000000000000000000000000000001")),
      BigDecimal("10e1000000"),
      BigDecimal("10e-1000000")
    )
    assert(
      valids.forall(_.isDecimalDouble) &&
      invalids.forall(! _.isDecimalDouble)
    )
  }
  
  // Motivated by SI-6173: BigDecimal#isWhole implementation is very heap intensive
  @Test
  def doesNotExplodeTest() {
    val troublemaker = BigDecimal("1e1000000000")
    val reasonable = BigDecimal("1e1000")
    val reasonableInt = reasonable.toBigInt
    assert(
      reasonable.hashCode == reasonableInt.hashCode &&
      reasonable == reasonableInt &&
      reasonableInt == reasonable &&
      troublemaker.hashCode != reasonable.hashCode &&
      !(troublemaker == reasonableInt) &&
      !(reasonableInt == troublemaker)
    )
  }
  
  // Motivated by SI-6456: scala.math.BigDecimal should not accept a null value
  @Test
  def refusesNullTest() {
    def isIAE[A](a: => A) = try { a; false } catch { case iae: IllegalArgumentException => true }
    def isNPE[A](a: => A) = try { a; false } catch { case npe: NullPointerException => true }
    assert(
      isIAE(new BigDecimal(null: BD, new MC(2))) &&
      isIAE(new BigDecimal(new BD("5.7"), null: MC)) &&
      isNPE(BigDecimal(null: BigInt)) &&
      isNPE(BigDecimal(null: String)) &&
      isNPE(BigDecimal(null: Array[Char]))
    )
  }
  
  // Motivated by SI-6153: BigDecimal.hashCode() has high collision rate
  @Test
  def hashCodesAgreeTest() {
    val bi: BigInt = 100000
    val bd: BigDecimal = 100000
    val l: Long = 100000
    val d: Double = 100000
    assert(
      d.## == l.## &&
      l.## == bd.## &&
      bd.## == bi.## &&
      (bd pow 4).hashCode == (bi pow 4).hashCode &&
      BigDecimal("1e150000").hashCode != BigDecimal("1e150000").toBigInt.hashCode
    )
  }
  
  // Motivated by noticing BigDecimal(0.1f) != BigDecimal(0.1)
  @Test
  def consistentTenthsTest() {
    def tenths = List[Any](
      BigDecimal("0.1"),
      0.1,
      BigDecimal.decimal(0.1f),
      BigDecimal.decimal(0.1),
      BigDecimal(0.1),
      BigDecimal(BigInt(1), 1),
      BigDecimal(new BD("0.1")),
      BigDecimal(1L, 1),
      BigDecimal(1) / BigDecimal(10),
      BigDecimal(10).pow(-1)
    )
    for (a <- tenths; b <- tenths) assert(a == b, s"$a != $b but both should be 0.1")
  }
  
  // Motivated by noticing BigDecimal(123456789, mc6) != BigDecimal(123456789L, mc6)
  // where mc6 is a MathContext that rounds to six digits
  @Test
  def consistentRoundingTest() {
    val mc6 = new MC(6)
    val sameRounding = List(
      List(
        123457000,
        123457000L,
        123457e3,
        BigDecimal(123456789, mc6),
        BigDecimal(123456789L, mc6),
        BigDecimal(123456789d, mc6),
        BigDecimal("123456789", mc6),
        BigDecimal(Array('1','2','3','4','5','6','7','8','9'), mc6),
        BigDecimal(BigInt(123456789), mc6),
        BigDecimal(BigInt(1234567890), 1, mc6),
        BigDecimal.decimal(123456789, mc6),
        BigDecimal.decimal(123456789d, mc6),
        BigDecimal.decimal(new BD("123456789"), mc6)
      ),
      List(
        123456789,
        123456789L,
        123456789d,
        new BigDecimal(new BD("123456789"), mc6),
        new BigDecimal(new BD("123456789")),
        BigDecimal(123456789),
        BigDecimal(123456789L),
        BigDecimal(123456789d),
        BigDecimal("123456789"),
        BigDecimal(Array('1','2','3','4','5','6','7','8','9')),
        BigDecimal(BigInt(123456789)),
        BigDecimal(BigInt(1234567890), 1),
        BigDecimal.decimal(123456789),
        BigDecimal.decimal(123456789d),
        BigDecimal.valueOf(123456789d, mc6) 
      )
    )
    sameRounding.map(_.zipWithIndex).foreach{ case xs => 
      for ((a,i) <- xs; (b,j) <- xs) {
        assert(a == b, s"$a != $b (#$i != #$j) but should be the same")
        assert(a.## == b.##, s"Hash code mismatch in equal BigDecimals: #$i != #$j")
      }
    }
    val List(xs, ys) = sameRounding.map(_.zipWithIndex)
    for ((a,i) <- xs; (b,j) <- ys) assert(a != b, s"$a == $b (#$i == #$j) but should be different")
  } 
 
  // This was unexpectedly truncated in 2.10
  @Test
  def noPrematureRoundingTest() {
    val text = "9791375983750284059237954823745923845928547807345082378340572986452364"
    val same = List[Any](
      BigInt(text), BigDecimal(text), BigDecimal(new BD(text))
    )
    for (a <- same; b <- same) assert(a == b, s"$a != $b but should be the same")
  }
  
  // Tests attempts to make sane the representation of IEEE binary32 and binary64
  // (i.e. Float and Double) with Scala's text-is-King BigDecimal policy
  @Test
  def churnRepresentationTest() {
    val rn = new scala.util.Random(42)
    for (i <- 1 to 1000) {
      val d = rn.nextDouble
      assert({
        BigDecimal.decimal(d).isDecimalDouble &&
        BigDecimal.binary(d).isBinaryDouble &&
        BigDecimal.exact(d).isExactDouble
      }, s"At least one wrong BigDecimal representation for $d")
    }
    for (i <- 1 to 1000) {
      val f = rn.nextFloat
      assert({
        BigDecimal.decimal(f).isDecimalFloat &&
        BigDecimal.binary(f).isBinaryFloat &&
        BigDecimal.exact(f).isExactFloat
      }, s"At least one wrong BigDecimal representation for $f")
    }
    for (i <- 1 to 1000) {
      val ndig = 15+rn.nextInt(5)
      val s = Array.fill(ndig)((rn.nextInt(10)+'0').toChar).mkString
      val bi = BigInt(s)
      val l = bi.toLong
      val d = bi.toDouble
      val bd = BigDecimal(bi)
      val bd2 = BigDecimal.decimal(d)
      assert(!bi.isValidLong || bi == l, s"Should be invalid or equal: $bi $l")
      assert(!bi.isValidDouble || bi == d, s"Should be invalid or equal: $bi $d")
      assert(bd == bi, s"Should be equal $bi $bd")
      assert(bd.## == bi.##, s"Hash codes for $bi, $bd should be equal")
      assert(bd == bd2 || bd2 != BigDecimal.exact(d) || !bi.isValidDouble,
        s"$bd != $bd2 should only be when inexact or invalid")
      assert(d == bd2 && bd2 == d, s"$d != $bd2 but they should equal")
    }
    val different = List(
      BigDecimal.decimal(0.1),
      BigDecimal.binary(0.1),
      BigDecimal.binary(0.1, new MC(25)),
      BigDecimal.exact(0.1),
      BigDecimal.exact(0.1f),
      BigDecimal.decimal((0.1f).toDouble)
    )
    for (a <- different; b <- different if (a ne b))
      assert(a != b, "BigDecimal representations of Double mistakenly conflated")
  }

  // Make sure hash code agrees with decimal representation of Double
  @Test
  def test_SI8970() {
    assert((0.1).## == BigDecimal(0.1).##)
  }

  // Motivated by the problem of MathContext lost
  @Test
  def testMathContext() {
    def testPrecision() {
      val p = 1000
      val n = BigDecimal("1.1", MC.UNLIMITED).pow(p)

      // BigDecimal(x: Float, mc: MC), which may not do what you want, is deprecated
      assert(BigDecimal(1.1f, MC.UNLIMITED).pow(p) == BigDecimal(java.lang.Double.toString(1.1f.toDouble), MC.UNLIMITED).pow(p))
      assert(BigDecimal(1.1d, MC.UNLIMITED).pow(p) == n)
      assert(BigDecimal(new BD("1.1"), MC.UNLIMITED).pow(p) == n)

      assert(BigDecimal.decimal(1.1f, MC.UNLIMITED).pow(p) == n)
      assert(BigDecimal.decimal(1.1d, MC.UNLIMITED).pow(p) == n)
      assert(BigDecimal.decimal(new BD("1.1"), MC.UNLIMITED).pow(p) == n)

      assert((BigDecimal(11, MC.UNLIMITED) / 10).pow(p) == n)
      assert((BigDecimal.decimal(11, MC.UNLIMITED) / 10).pow(p) == n)
    }

    def testRounded() {
      // the default rounding mode is HALF_UP
      assert((BigDecimal(1.23f, new MC(3)) + BigDecimal("0.005")).rounded == BigDecimal("1.24")) // deprecated api
      assert((BigDecimal(1.23d, new MC(3)) + BigDecimal("0.005")).rounded == BigDecimal("1.24"))
      assert((BigDecimal.decimal(1.23f, new MC(3)) + BigDecimal("0.005")).rounded == BigDecimal("1.24"))
      assert((BigDecimal.decimal(1.23d, new MC(3)) + BigDecimal("0.005")).rounded == BigDecimal("1.24"))
    }

    testPrecision()
    testRounded()
  }
}
