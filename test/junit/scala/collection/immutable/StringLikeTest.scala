package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil
import scala.util.Random

/* Test for SI-8988 */
@RunWith(classOf[JUnit4])
class StringLikeTest {
  @Test
  def testStringSplitWithChar: Unit = {
    val chars = (0 to 255).map(_.toChar)
    def randString = Random.nextString(30)

    for (c <- chars) {
      val s = randString
      val jString = new java.lang.String(s)

      // make sure we can match a literal character done by Java's split
      val jSplit = jString.split("\\Q" + c.toString + "\\E")
      val sSplit = s.split(c)
      AssertUtil.assertSameElements(jSplit, sSplit, s"Not same result as Java split for char $c in string $s")
    }
  }

  @Test
  def testSplitEdgeCases: Unit = {
    val high = 0xD852.toChar
    val low = 0xDF62.toChar
    val surrogatepair = List(high, low).mkString
    val twopairs = surrogatepair + "_" + surrogatepair
    
    AssertUtil.assertSameElements("abcd".split('d'), Array("abc")) // not Array("abc", "")
    AssertUtil.assertSameElements("abccc".split('c'), Array("ab")) // not Array("ab", "", "", "")
    AssertUtil.assertSameElements("xxx".split('x'), Array[String]()) // not Array("", "", "", "")
    AssertUtil.assertSameElements("".split('x'), Array("")) // not Array()
    AssertUtil.assertSameElements("--ch--omp--".split("-"), Array("", "", "ch", "", "omp")) // All the cases!
    AssertUtil.assertSameElements(twopairs.split(high), Array(twopairs)) //don't split on characters that are half a surrogate pair
  }

  /* Test for SI-9767 */
  @Test
  def testNumericConversion: Unit = {
    val sOne = " \t\n 1 \n\r\t "
    val sNull:String = null

    assertTrue("trim toInt", sOne.toInt == 1)
    assertTrue("trim toLong", sOne.toLong == 1L)
    assertTrue("trim toShort", sOne.toShort == 1.toShort)
    assertTrue("trim toByte", sOne.toByte == 1.toByte)
    assertTrue("trim toDouble", sOne.toDouble == 1.0d) // was working before issue
    assertTrue("trim toFloat", sOne.toFloat == 1.0f) // was working before issue
    AssertUtil.assertThrows[java.lang.NumberFormatException](sNull.toInt, {s => s == "null"}) // was working before issue
    AssertUtil.assertThrows[java.lang.NumberFormatException](sNull.toLong, {s => s == "null"}) // was working before issue
    AssertUtil.assertThrows[java.lang.NumberFormatException](sNull.toShort, {s => s == "null"}) // was working before issue
    AssertUtil.assertThrows[java.lang.NumberFormatException](sNull.toByte, {s => s == "null"}) // was working before issue
    AssertUtil.assertThrows[java.lang.NumberFormatException](sNull.toDouble, {s => s == "null"})
    AssertUtil.assertThrows[java.lang.NumberFormatException](sNull.toFloat, {s => s == "null"})
  }
}
