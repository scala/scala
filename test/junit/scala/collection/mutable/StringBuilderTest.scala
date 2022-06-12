package scala.collection.mutable

import org.junit.Assert._
import org.junit.Test

class StringBuilderTest {
  @Test
  def specificBuilder(): Unit = {
    val b = new StringBuilder() ++= "abcd"
    val b1 = b.filter(_ % 2 == 0)
    val b1t: StringBuilder = b1
    assertEquals("bd", b1t.toString)

    val b2 = b.map(c => (c + 1).toChar)
    val b2t: IndexedSeq[Char] = b2
    assertEquals("ArrayBuffer(b, c, d, e)", b2t.toString)
  }

  @Test
  def testToArray(): Unit = {
    val b = new StringBuilder("ab")
    assertArrayEquals(Array('a', 'b'), b.toCharArray)
    assertArrayEquals(Array('a', 'b'), b.toArray)
  }

  @Test def `mutating ops return this builder`: Unit = {
    val b = new StringBuilder()
    val res0: b.type = b.addOne('a')
    val res1: b.type = res0.append("b")
    val res2: b.type = res1.append("c": Any)
    val res = res2
    assertEquals("abc", res.toString)
    assertSame(b, res)
  }

  @Test def `addString returns this builder`: Unit = {
    val src = new StringBuilder("abc")
    val b = new StringBuilder()
    val res: b.type = src.addString(b, "X", ",", "Y")
    assertEquals("Xa,b,cY", res.toString)
    assertSame(b, res)
  }
}
