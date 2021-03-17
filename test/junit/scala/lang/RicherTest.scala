
package scala

import org.junit.{Assert, Test}
import scala.util.chaining._

class RicherTest {
  import RicherTest._

  private def assertEqualTo(expected: String)(actual: String) = Assert.assertEquals(expected, actual)
  private def assertEqualTo(expected: Int)(actual: Int) = Assert.assertEquals(expected, actual)
  private def assertEqualTo[A](expected: List[A])(actual: List[A]) = Assert.assertEquals(expected, actual)

  @Test def `Byte expansions should be byte-sized`(): Unit = {
    val sixteen = 16.toByte
    assertEqualTo(x"1_0000")(sixteen.toBinaryString)
    assertEqualTo("10")(sixteen.toHexString)
    assertEqualTo("20")(sixteen.toOctalString)
    val max = 0x7F.toByte
    assertEqualTo(x"111_1111")(max.toBinaryString)
    assertEqualTo("7f")(max.toHexString)
    assertEqualTo("177")(max.toOctalString)
    val extended = 0x80.toByte
    assertEqualTo("1" * 24 + x"1000_0000")(extended.toBinaryString)
    assertEqualTo(x"ffff_ff80")(extended.toHexString)
    assertEqualTo("37777777600")(extended.toOctalString)
    val neg = -1.toByte
    assertEqualTo("1" * 32)(neg.toBinaryString)
    assertEqualTo("f" * 8)(neg.toHexString)
    assertEqualTo("3" + "7" * 10)(neg.toOctalString)
  }
  @Test def `Short expansions should be short-sized`(): Unit = {
    val sixteen = 16.toShort
    assertEqualTo(x"1_0000")(sixteen.toBinaryString)
    assertEqualTo("10")(sixteen.toHexString)
    assertEqualTo("20")(sixteen.toOctalString)
    val max = 0x7FFF.toShort
    assertEqualTo(x"111_1111_1111_1111")(max.toBinaryString)
    assertEqualTo("7fff")(max.toHexString)
    assertEqualTo("77777")(max.toOctalString)
    val extended = 0x8000.toShort
    assertEqualTo(x"1111_1111_1111_1111_1000_0000_0000_0000")(extended.toBinaryString)
    assertEqualTo(x"ffff_8000")(extended.toHexString)
    assertEqualTo(x"37777700000")(extended.toOctalString)
    val neg = -1.toShort
    assertEqualTo("1" * 32)(neg.toBinaryString)
    assertEqualTo(x"ffff_ffff")(neg.toHexString)
    assertEqualTo(x"37777777777")(neg.toOctalString)
  }
  // same as short, but uses int conversion because unsigned
  @Test def `Char expansions should be char-sized`(): Unit = {
    val sixteen = 16.toChar
    assertEqualTo(x"1_0000")(sixteen.toBinaryString)
    assertEqualTo("10")(sixteen.toHexString)
    assertEqualTo("20")(sixteen.toOctalString)
    val max = 0x7FFF.toChar
    assertEqualTo(x"111_1111_1111_1111")(max.toBinaryString)
    assertEqualTo("7fff")(max.toHexString)
    assertEqualTo("77777")(max.toOctalString)
    val extended = 0x8000.toChar
    assertEqualTo(x"1000_0000_0000_0000")(extended.toBinaryString)
    assertEqualTo("8000")(extended.toHexString)
    assertEqualTo(x"10_0000")(extended.toOctalString)
    val neg = -1.toChar
    assertEqualTo("1" * 16)(neg.toBinaryString)
    assertEqualTo("ffff")(neg.toHexString)
    assertEqualTo(x"17_7777")(neg.toOctalString)
  }
  @Test def `Chars are digits`(): Unit = {
    assertEqualTo(1)('1'.asDigit)
    assertEqualTo(10)('A'.asDigit)
  }
  @Test def `Ints are ranged`(): Unit = {
    assertEqualTo(10)((0 until 10).length)
    assertEqualTo(11)((0 to 10).length)
    assertEqualTo(12)((-2 until 10).length)
    assertEqualTo(13)((-2 to 10).length)
    assertEqualTo(0)((10 until -2).length)
    assertEqualTo(0)((10 to -2).length)
  }
  @Test def `Int strings`(): Unit = {
    assertEqualTo(x"1_0000")(16.toBinaryString)
    assertEqualTo("10")(16.toHexString)
    assertEqualTo("20")(16.toOctalString)
    assertEqualTo("10001")(65537.toHexString)
    assertEqualTo("f" * 8)(-1.toHexString)
  }

  // see also StringLikeTest
  val s1 = """abc"""
  val s2 = """abc\txyz\n"""
  val s3 = """abc
              xyz"""
  val s4 = """abc
              |xyz"""
  val s5 = """abc
              #xyz"""
  @Test def `linesIterator iterates lines`(): Unit = {
    assertEqualTo(1)(s1.linesIterator.length)
    assertEqualTo(s1)(s1.linesIterator.next())
    assertEqualTo(1)(s2.linesIterator.length)
    assertEqualTo(s2)(s2.linesIterator.next())
    assertEqualTo(2)(s3.linesIterator.length)
    assertEqualTo("abc")(s3.linesIterator.next())
    assertEqualTo("              xyz")(s3.linesIterator.pipe { it => it.next(); it.next() })
  }
  @Test def `stripLineEnd strips lines ends`(): Unit = {
    assertEqualTo(s1)(s1.stripLineEnd)
    assertEqualTo(s2)(s2.stripLineEnd)
    assertEqualTo(s3)(s3.stripLineEnd)
    assertEqualTo(s4)(s4.stripLineEnd)
    assertEqualTo(s5)(s5.stripLineEnd)
    assertEqualTo("abc")("abc\n".stripLineEnd)
  }
  @Test def `stripMargin strips lines margins`(): Unit = {
    assertEqualTo(s1)(s1.stripMargin)
    assertEqualTo(s2)(s2.stripMargin)
    assertEqualTo(s3)(s3.stripMargin)
    assertEqualTo("abc\nxyz")(s4.stripMargin)
    assertEqualTo(s5)(s5.stripMargin)
  }
  @Test def `stripMargin strips custom margins`(): Unit = {
    assertEqualTo(s1)(s1.stripMargin('#'))
    assertEqualTo(s2)(s2.stripMargin('#'))
    assertEqualTo(s3)(s3.stripMargin('#'))
    assertEqualTo(s4)(s4.stripMargin('#'))
    assertEqualTo("abc\nxyz")(s5.stripMargin('#'))
  }
  @Test def `split splits strings`(): Unit = {
    assertEqualTo(List("a","b","c","d"))("a:b:c:d".split(':').toList)
    assertEqualTo(List("a","b","c","d"))("a.b.c.d".split('.').toList)
    assertEqualTo(List("a","b","c","d"))("a$b$c$d".split('$').toList)
    assertEqualTo(List("a","b","c","d"))("a^b^c^d".split('^').toList)
    assertEqualTo(List("a","b","c","d"))("a\\b\\c\\d".split('\\').toList)
    assertEqualTo(List("a","b","c","d"))("a:b:c.d".split(Array(':','.')).toList)
    assertEqualTo(List("a","b","c","d"))("a:b.c$d".split(Array(':','.','$')).toList)
  }
}

object RicherTest {
  implicit class stripper(private val sc: StringContext) extends AnyVal {
    def x(args: Any*) = StringContext.standardInterpolator(_.replace("_", ""), args, sc.parts)
  }
}
