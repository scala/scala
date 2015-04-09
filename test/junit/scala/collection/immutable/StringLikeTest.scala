package scala.collection.immutable

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
    AssertUtil.assertSameElements("abcd".split('d'), Array("abc")) // not Array("abc", "")
    AssertUtil.assertSameElements("abccc".split('c'), Array("ab")) // not Array("ab", "", "", "")
    AssertUtil.assertSameElements("xxx".split('x'), Array[String]()) // not Array("", "", "", "")
    AssertUtil.assertSameElements("".split('x'), Array("")) // not Array()
    AssertUtil.assertSameElements("--ch--omp--".split("-"), Array("", "", "ch", "", "omp")) // All the cases!
  }
}
