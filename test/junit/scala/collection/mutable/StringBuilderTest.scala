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
}
