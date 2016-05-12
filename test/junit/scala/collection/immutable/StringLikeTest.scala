package scala.collection.immutable

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil._
import scala.util.Random

@RunWith(classOf[JUnit4])
class StringLikeTest {
  /* Test for SI-8988 */
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
      assertSameElements(jSplit, sSplit, s"Not same result as Java split for char $c in string $s")
    }
  }

  @Test
  def testSplitEdgeCases: Unit = {
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

  @Test def `linesWithSeparators is like split.iterator`(): Unit = {
    assertEquals(1, "".linesWithSeparators.size)
    assertEquals("", "".linesWithSeparators.next)
    assertSameElements(List("a\n", "b"), "a\nb".linesWithSeparators.toList)
    assertSameElements(List("a\n", "b\n"), "a\nb\n".linesWithSeparators.toList)
    assertSameElements(List("\n", "a\n", "b\n"), "\na\nb\n".linesWithSeparators.toList)
    assertSameElements(List("a\r\n", "b"), "a\r\nb".linesWithSeparators.toList)
    assertSameElements(List("a\f", "b"), "a\fb".linesWithSeparators.toList)
  }
}
