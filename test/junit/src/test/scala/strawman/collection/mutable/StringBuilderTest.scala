package strawman.collection.mutable

import org.junit.Assert.assertEquals
import strawman.collection.immutable.List
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import scala.tools.testing.AssertUtil

@RunWith(classOf[JUnit4])
class StringBuilderTest {
  @Test
  def specificBuilder(): Unit = {
    val b = new StringBuilder() ++= "abcd"
    val b1 = b.filter(_ % 2 == 0)
    val b1t: StringBuilder = b1
    assertEquals(b1t.toString, "bd")

    val b2 = b.map(c => (c + 1).toChar)
    val b2t: IndexedSeq[Char] = b2
    assertEquals(b2t.toString, "ArrayBuffer(b, c, d, e)")
  }
}
