package strawman.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.Predef.{ $conforms, classOf }
import strawman.collection.immutable.List

@RunWith(classOf[JUnit4])
class ArrayOpsTest {

  @Test
  def unzip(): Unit = {
    val zipped = Array((1, 'a'), (2, 'b'), (3, 'c'))

    val (a1, a2) = zipped.unzip

    assertArrayEquals(Array(1, 2, 3), a1)
    assertArrayEquals(Array('a', 'b', 'c'), a2)
  }

  @Test
  def unzip3(): Unit = {
    val zipped = Array((1, 'a', true), (2, 'b', false), (3, 'c', true))
    val (a1, a2, a3) = zipped.unzip3
    assertArrayEquals(Array(1, 2, 3), a1)
    assertArrayEquals(Array('a', 'b', 'c'), a2)
    assertTrue(Array(true, false, true).sameElements(a3))
  }

  @Test
  def reverseIterator: Unit = {
    val a = Array(1,2,3)
    assertEquals(List(3,2,1), a.reverseIterator.toList)
  }
}
