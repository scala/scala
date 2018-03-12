package strawman.collection

import org.junit.Assert.{ assertArrayEquals,assertTrue }
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.Predef.{ refArrayOps => _, genericArrayOps => _, genericWrapArray => _, wrapRefArray => _, _ }
import strawman.collection.arrayToArrayOps

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
}
