package scala.collection.immutable

import org.junit.jupiter.api.Assertions.{ assertThrows => _, _ }
import org.junit.jupiter.api.Test

import scala.tools.testkit.AssertUtil._
import scala.util.Random

/* Test for scala/bug#8988 */
class StringLikeTest {
  @Test
  def testStringSplitWithChar(): Unit = {
    val chars = (0 to 255).map(_.toChar)
    def randString = Random.nextString(30)

    for (c <- chars) {
      val s = randString
      val jString = new java.lang.String(s)

      // make sure we can match a literal character done by Java's split
      val jSplit = jString.split("\\Q" + c.toString + "\\E")
      val sSplit = s.split(c)
      assertSameElements(jSplit, sSplit, s"Not same result as Java split for char $c in string $s")
    }
  }

  @Test
  def testSplitEdgeCases(): Unit = {
    val high = 0xD852.toChar
    val low = 0xDF62.toChar
    val surrogatepair = List(high, low).mkString
    val twopairs = surrogatepair + "_" + surrogatepair
    
    assertSameElements("abcd".split('d'), Array("abc")) // not Array("abc", "")
    assertSameElements("abccc".split('c'), Array("ab")) // not Array("ab", "", "", "")
    assertSameElements("xxx".split('x'), Array[String]()) // not Array("", "", "", "")
    assertSameElements("".split('x'), Array("")) // not Array()
    assertSameElements("--ch--omp--".split("-"), Array("", "", "ch", "", "omp")) // All the cases!
    assertSameElements(twopairs.split(high), Array(twopairs)) //don't split on characters that are half a surrogate pair
  }

  /* Test for scala/bug#9767 */
  @Test
  def testNumericConversion(): Unit = {
    val sOne = " \t\n 1 \n\r\t "
    val sOk  = "2"
    val sNull:String = null

    assertThrows[java.lang.NumberFormatException](sOne.toInt)
    assertThrows[java.lang.NumberFormatException](sOne.toLong)
    assertThrows[java.lang.NumberFormatException](sOne.toShort)
    assertThrows[java.lang.NumberFormatException](sOne.toByte)
    assertEquals(1.0d, sOne.toDouble, 0.1d, "trim toDouble")
    assertEquals(1.0d, sOne.toDouble, 0.1d, "trim toDouble")
    assertEquals(1.0f, sOne.toFloat, 0.1f, "trim toFloat")

    assertEquals(2, sOk.toInt, "no trim toInt")
    assertEquals(2L, sOk.toLong, "no trim toLong")
    assertEquals(2.toShort, sOk.toShort, "no trim toShort")
    assertEquals(2.toByte, sOk.toByte, "no trim toByte")
    assertEquals(2.0d, sOk.toDouble, 0.1d, "no trim toDouble")
    assertEquals(2.0f, sOk.toFloat, 0.1f, "no trim toFloat")

    // JDK 17 gives the nicer message
    def isNullStringMessage(s: String) =
      s == "null" || s == "Cannot parse null string"

    assertThrows[java.lang.NumberFormatException](sNull.toInt, isNullStringMessage)
    assertThrows[java.lang.NumberFormatException](sNull.toLong, isNullStringMessage)
    assertThrows[java.lang.NumberFormatException](sNull.toShort, isNullStringMessage)
    assertThrows[java.lang.NumberFormatException](sNull.toByte, isNullStringMessage)

    assertThrows[java.lang.NullPointerException](sNull.toDouble)
    assertThrows[java.lang.NullPointerException](sNull.toFloat)
  }

  @Test
  def `line split on CR`(): Unit = {
    assertEquals(2, "abc\r\ndef".linesIterator.size)
    assertEquals(2, "abc\rdef".linesIterator.size)
  }

  @Test
  def `line split on NL, FF`(): Unit = {
    assertEquals(2, "abc\ndef".linesIterator.size)
    assertEquals(1, "abc\fdef".linesIterator.size)     // no more form feed splitting
    assertEquals(2, "abc\ndef\n".linesIterator.size)

    // previous status quo
    assertEquals(2, "abc\n\fdef".linesIterator.size)
    assertEquals(3, "abc\n\f\ndef".linesIterator.size)

    assertSameElements(List("abc", "def"), "abc\ndef".linesIterator)
  }

  @Test
  def `strip line endings`(): Unit = {
    assertEquals("abc", "abc".stripLineEnd)
    assertEquals("abc", "abc\n".stripLineEnd)
    assertEquals("abc\n", "abc\n\n".stripLineEnd)
    assertEquals("abc", "abc\r\n".stripLineEnd)
    assertEquals("abc\r\n\f", "abc\r\n\f".stripLineEnd)  // no more form feed stripping
    assertEquals("abc\f", "abc\f".stripLineEnd)
  }
}
