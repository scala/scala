package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

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

  @Test
  def folds: Unit = {
    val a = Array(1,2,3)
    assertEquals(6, a.foldLeft(0){ (a, b) => a+b })
    assertEquals(6, a.foldRight(0){ (a, b) => a+b })
    assertEquals(6, a.fold(0){ (a, b) => a+b })
  }
}
