package strawman.collection

import scala.Predef.{augmentString => _, classOf, charWrapper}

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class IndexedSeqOptimizedTest {

  @Test
  def notThrowsAnExceptionInLastIndexOf(): Unit = {
    assertEquals(0, Array(2).lastIndexWhere(_ => true, 1))
    assertEquals(2, "abc123".lastIndexWhere(_.isLetter, 6))
  }

  @Test
  def hasCorrectDropAndTakeMethods(): Unit = {
    assertEquals("", "abc" take Int.MinValue)
    assertEquals("", "abc" takeRight Int.MinValue)
    assertEquals("abc", "abc" drop Int.MinValue)
    assertEquals("abc", "abc" dropRight Int.MinValue)

    assertArrayEquals(Array.empty[Int], Array(1, 2, 3) take Int.MinValue)
    assertArrayEquals(Array.empty[Int], Array(1, 2, 3) takeRight Int.MinValue)
    assertArrayEquals(Array(1, 2, 3), Array(1, 2, 3) drop Int.MinValue)
    assertArrayEquals(Array(1, 2, 3), Array(1, 2, 3) dropRight Int.MinValue)
  }
}
