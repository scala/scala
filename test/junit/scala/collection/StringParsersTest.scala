package scala.collection

import org.junit.{Assert, Test}
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.tools.testkit.AssertUtil._
import scala.util.Try

@RunWith(classOf[JUnit4])
class StringParsersTest {

  def doubleOK(str: String): Unit = assertTrue(
    s"str.toDouble <> str.toDoubleOption for $str",
    (str.toDoubleOption, Try(str.toDouble).toOption) match {
      case (Some(d1), Some(d2)) => d1.isNaN && d2.isNaN || d1 == d2
      case (o1, o2) => o1 == o2
    })

  def floatOK(str: String): Unit = assertTrue(
    s"str.toFloat <> str.toFloatOption for $str",
    (str.toFloatOption, Try(str.toFloat).toOption) match {
      case (Some(f1), Some(f2)) if f1.isNaN && f2.isNaN => true
      case (o1, o2) => o1 == o2
    })

  def byteOK(str: String): Unit = assertTrue(
    s"str.toByte <> str.toByteOption for $str",
    str.toByteOption == Try(str.toByte).toOption)

  def shortOK(str: String): Unit = assertTrue(
    s"str.toShort <> str.toShortOption for $str",
    str.toShortOption == Try(str.toShort).toOption)

  def intOK(str: String): Unit = assertTrue(
    s"str.toInt <> str.toIntOption for $str",
    str.toIntOption == Try(str.toInt).toOption)

  def longOK(str: String): Unit = assertTrue(
    s"str.toLong <> str.toLongOption for $str",
    str.toLongOption == Try(str.toLong).toOption)

  val forAllExamples = List("", "+", "-", "0", "-0", "+0", "1", "-1", "+1")

  val nearOverflow = for {
    b <- List(Byte.MinValue, Byte.MaxValue, Short.MinValue, Short.MaxValue, Int.MinValue, Int.MaxValue)
    l = b.toLong
    d <- (-10 to 10)
    ii <- List(l + d)
  } yield ii.toString

  val noLongOverflow = List(Long.MinValue, Long.MinValue + 1, Long.MaxValue, Long.MaxValue - 1)

  val longOverUnderflow = List("9223372036854775808", "-9223372036854775809")

  val longNearOverflow = noLongOverflow.map(_.toString) ::: longOverUnderflow

  val nullstring: String = null

  //test cases taken from Apache Harmony: https://android.googlesource.com/platform/libcore/+/master/harmony-tests/src/test/java/org/apache/harmony/tests/java/lang/DoubleTest.java
  val doubleExamples = List(
    "-1.233999999999999965116738099630936817275852021384209929081813042837802886790127428328465579708849276001782791006814286802871737087810957327493372866733334925806221045495205250590286471187577636646208155890426896101636282423463443661040209738873506655844025580428394216030152374941053494694642722606658935546875E-112",
    "2.4703282292062327208828439643411e-324",
    "2.4703282292062327208828439643412e-324",
    "3.4e-0",
    "3.4e-1",
    "3.4e-323",
    "3.4e-324",
    "1.2e0",
    "1.2e308",
    "1.2e309",
    "1.2e310",
    "3.4e-324",
    "0.0p0D",
    "+0x.p1d",
    "0Xg.gp1D",
    "-0x1.1p",
    "+0x 1.1 p2d",
    "x1.1p2d",
    " 0x-2.1p2",
    " 0x2.1pad",
    " 0x111.222p 22d",
    "0x0.0p0D",
    "0xa.ap+9d",
    "+0Xb.10ap8",
    "-0X.a0P2D",
    "\r 0x22.1p2d \t",
    "0x1.0p-1",
    "0x00000000000000000000000000000000001.0p-1",
    "0x1.0p-00000000000000000000000000001",
    "0x.100000000000000000000000000000000p1",
    "0x0.0p999999999999999999999999999999999999999999999999999999999999999",
    "0xf1.0p9999999999999999999999999999999999999999999999999999999999999999",
    "0xffffffffffffffffffffffffffffffffffff.ffffffffffffffffffffffffffffffffffffffffffffffp1",
    "0x0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001p1600",
    "0x0.0p-999999999999999999999999999999999999999999999999999999",
    "0xf1.0p-9999999999999999999999999999999999999999999999999999999999999999",
    "0x10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000p-1600",
    "0x1.p9223372036854775807",
    "0x1.p9223372036854775808",
    "0x10.p9223372036854775808",
    "0xabcd.ffffffffp+2000",
    "0x1.p-9223372036854775808",
    "0x1.p-9223372036854775809",
    "0x.1p-9223372036854775809",
    "0xabcd.ffffffffffffffp-2000",
    "0x1.fffffffffffffp1023",
    "0x1.fffffffffffff000000000000000000000000001p1023",
    "0x1.fffffffffffff1p1023",
    "0x1.fffffffffffff100000000000000000000000001p1023",
    "0x1.fffffffffffff1fffffffffffffffffffffffffffffffffffffffffffffp1023",
    "0x1.fffffffffffff7p1023",
    "0x1.fffffffffffff700000000000000000000000001p1023",
    "0x1.fffffffffffff8p1023",
    "0x1.fffffffffffff800000000000000000000000001p1023",
    "0x1.fffffffffffff8fffffffffffffffffffffffffffffffffffffffffffffp1023",
    "0x1.fffffffffffff9p1023",
    "0x1.fffffffffffff900000000000000000000000001p1023",
    "0x1.ffffffffffffffp1023",
    "0x1.ffffffffffffff00000000000000000000000001p1023",
    "0x1.fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffp1023",
    "-0x1.fffffffffffffp1023",
    "-0x1.fffffffffffff000000000000000000000000001p1023",
    "-0x1.fffffffffffff1p1023",
    "-0x1.fffffffffffff100000000000000000000000001p1023",
    "-0x1.fffffffffffff1fffffffffffffffffffffffffffffffffffffffffffffp1023",
    "-0x1.fffffffffffff7p1023",
    "-0x1.fffffffffffff700000000000000000000000001p1023",
    "-0x1.fffffffffffff8p1023",
    "-0x1.fffffffffffff800000000000000000000000001p1023",
    "-0x1.fffffffffffff8fffffffffffffffffffffffffffffffffffffffffffffp1023",
    "-0x1.fffffffffffff9p1023",
    "-0x1.fffffffffffff900000000000000000000000001p1023",
    "-0x1.ffffffffffffffp1023",
    "-0x1.ffffffffffffff00000000000000000000000001p1023",
    "-0x1.fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffp1023",
    "0x1.0p-1022",
    "0x1.00000000000001p-1022",
    "0x1.000000000000010000000000000000001p-1022",
    "0x1.00000000000001fffffffffffffffffffffffffffffffffp-1022",
    "0x1.00000000000007p-1022",
    "0x1.000000000000070000000000000000001p-1022",
    "0x1.00000000000007fffffffffffffffffffffffffffffffffp-1022",
    "0x1.00000000000008p-1022",
    "0x1.000000000000080000000000000000001p-1022",
    "0x1.00000000000008fffffffffffffffffffffffffffffffffp-1022",
    "0x1.00000000000009p-1022",
    "0x1.000000000000090000000000000000001p-1022",
    "0x1.00000000000009fffffffffffffffffffffffffffffffffp-1022",
    "0x1.0000000000000fp-1022",
    "0x1.0000000000000ffffffffffffffffffffffffffffffffffp-1022",
    "-0x1.0p-1022",
    "-0x1.00000000000001p-1022",
    "-0x1.000000000000010000000000000000001p-1022",
    "-0x1.00000000000001fffffffffffffffffffffffffffffffffp-1022",
    "-0x1.00000000000007p-1022",
    "-0x1.000000000000070000000000000000001p-1022",
    "-0x1.00000000000007fffffffffffffffffffffffffffffffffp-1022",
    "-0x1.00000000000008p-1022",
    "-0x1.000000000000080000000000000000001p-1022",
    "-0x1.00000000000008fffffffffffffffffffffffffffffffffp-1022",
    "-0x1.00000000000009p-1022",
    "-0x1.000000000000090000000000000000001p-1022",
    "-0x1.00000000000009fffffffffffffffffffffffffffffffffp-1022",
    "-0x1.0000000000000fp-1022",
    "-0x1.0000000000000ffffffffffffffffffffffffffffffffffp-1022",
    "0x0.fffffffffffffp-1022",
    "0x0.fffffffffffff00000000000000000000000000000000001p-1022",
    "0x0.fffffffffffff1p-1022",
    "0x0.fffffffffffff10000000000000000000000000000000001p-1022",
    "0x0.fffffffffffff1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffp-1022",
    "0x0.fffffffffffff7p-1022",
    "0x0.fffffffffffff7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffp-1022",
    "0x0.fffffffffffff8p-1022",
    "0x0.fffffffffffff80000000000000000000000000000000001p-1022",
    "0x0.fffffffffffff8ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffp-1022",
    "0x0.fffffffffffff9p-1022",
    "0x0.fffffffffffff9ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffp-1022",
    "0x0.ffffffffffffffp-1022",
    "0x0.ffffffffffffff0000000000000000000000000000000001p-1022",
    "0x0.ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffp-1022",
    "-0x0.fffffffffffffp-1022",
    "-0x0.fffffffffffff00000000000000000000000000000000001p-1022",
    "-0x0.fffffffffffff1p-1022",
    "-0x0.fffffffffffff10000000000000000000000000000000001p-1022",
    "-0x0.fffffffffffff1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffp-1022",
    "-0x0.fffffffffffff7p-1022",
    "-0x0.fffffffffffff7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffp-1022",
    "-0x0.fffffffffffff8p-1022",
    "-0x0.fffffffffffff80000000000000000000000000000000001p-1022",
    "-0x0.fffffffffffff8ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffp-1022",
    "-0x0.fffffffffffff9p-1022",
    "-0x0.fffffffffffff9ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffp-1022",
    "-0x0.ffffffffffffffp-1022",
    "-0x0.ffffffffffffff0000000000000000000000000000000001p-1022",
    "-0x0.ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffp-1022",
    "0x0.0000000000001p-1022",
    "0x0.00000000000010000000000000000001p-1022",
    "0x0.0000000000001fffffffffffffffffffffffffffffffffp-1022",
    "0x0.00000000000017p-1022",
    "0x0.000000000000170000000000000000001p-1022",
    "0x0.00000000000017fffffffffffffffffffffffffffffffffp-1022",
    "0x0.00000000000018p-1022",
    "0x0.000000000000180000000000000000001p-1022",
    "0x0.00000000000018fffffffffffffffffffffffffffffffffp-1022",
    "0x0.00000000000019p-1022",
    "0x0.000000000000190000000000000000001p-1022",
    "0x0.00000000000019fffffffffffffffffffffffffffffffffp-1022",
    "0x0.0000000000001fp-1022",
    "0x0.0000000000001f0000000000000000001p-1022",
    "0x0.0000000000001ffffffffffffffffffffffffffffffffffp-1022",
    "-0x0.0000000000001p-1022",
    "-0x0.00000000000010000000000000000001p-1022",
    "-0x0.0000000000001fffffffffffffffffffffffffffffffffp-1022",
    "-0x0.00000000000017p-1022",
    "-0x0.000000000000170000000000000000001p-1022",
    "-0x0.00000000000017fffffffffffffffffffffffffffffffffp-1022",
    "-0x0.00000000000018p-1022",
    "-0x0.000000000000180000000000000000001p-1022",
    "-0x0.00000000000018fffffffffffffffffffffffffffffffffp-1022",
    "-0x0.00000000000019p-1022",
    "-0x0.000000000000190000000000000000001p-1022",
    "-0x0.00000000000019fffffffffffffffffffffffffffffffffp-1022",
    "-0x0.0000000000001fp-1022",
    "-0x0.0000000000001f0000000000000000001p-1022",
    "-0x0.0000000000001ffffffffffffffffffffffffffffffffffp-1022",
    "0x0.00000000000004p-1022",
    "0x0.00000000000007ffffffffffffffffffffffp-1022",
    "0x0.00000000000008p-1022",
    "0x0.000000000000080000000000000000001p-1022",
    "0x0.00000000000008fffffffffffffffffffffffffffffffp-1022",
    "0x0.00000000000009p-1022",
    "0x0.000000000000090000000000000000001p-1022",
    "0x0.00000000000009fffffffffffffffffffffffffffffffffp-1022",
    "0x0.0000000000000fffffffffffffffffffffffffffffffffffp-1022",
    "-0x0.00000000000004p-1022",
    "-0x0.00000000000007ffffffffffffffffffffffp-1022",
    "-0x0.00000000000008p-1022",
    "-0x0.000000000000080000000000000000001p-1022",
    "-0x0.00000000000008fffffffffffffffffffffffffffffffp-1022",
    "-0x0.00000000000009p-1022",
    "-0x0.000000000000090000000000000000001p-1022",
    "-0x0.00000000000009fffffffffffffffffffffffffffffffffp-1022",
    "-0x0.0000000000000fffffffffffffffffffffffffffffffffffp-1022",
    "",
    ".",
    ".4",
    ".E4",
    ".E",
    ".x",
    ".1E4",
    "4.",
    "1.1E4",
    "1.E4",
    "1E4",
    "E4",
    "0.0",
    "+0.0",
    "-0.0",
    "NaN",
    "+NaN",
    "-NaN",
    "Infinity",
    "+Infinity",
    "-Infinity",
    "NaNd",
    "+NaNd",
    "-NaNd",
    "Infinityd",
    "+Infinityd",
    "-Infinityd"
  )

  @Test
  def doubleSpecificTest: Unit = doubleExamples.foreach(doubleOK)

  @Test
  def doubleGeneralTest: Unit = forAllExamples.foreach(doubleOK)

  @Test
  def floatSpecificTest: Unit = doubleExamples.foreach(floatOK)

  @Test
  def floatGeneralTest: Unit = forAllExamples.foreach(floatOK)

  @Test
  def byteTest: Unit = (forAllExamples ::: nearOverflow).foreach(byteOK)

  @Test
  def shortTest: Unit = (forAllExamples ::: nearOverflow).foreach(shortOK)

  @Test
  def intTest: Unit = (forAllExamples ::: nearOverflow).foreach(intOK)

  @Test
  def longTest: Unit = (forAllExamples ::: longNearOverflow).foreach(longOK)

  @Test
  def nullByte: Unit = assertThrows[NullPointerException](nullstring.toByteOption)

  @Test
  def nullShort: Unit = assertThrows[NullPointerException](nullstring.toShortOption)

  @Test
  def nullInt: Unit = assertThrows[NullPointerException](nullstring.toIntOption)
  
  @Test
  def nullLong: Unit = assertThrows[NullPointerException](nullstring.toLongOption)

  @Test
  def nullFloat: Unit = assertThrows[NullPointerException](nullstring.toFloatOption)

  @Test
  def nullDouble: Unit = assertThrows[NullPointerException](nullstring.toDoubleOption)

}
