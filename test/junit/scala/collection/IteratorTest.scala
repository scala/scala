
package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class IteratorTest {

  @Test
  def groupedIteratorShouldNotAskForUnneededElement(): Unit = {
    var counter = 0
    val it = new Iterator[Int] { var i = 0 ; def hasNext = { counter = i; true } ; def next = { i += 1; i } }
    val slidingIt = it sliding 2
    slidingIt.next
    assertEquals("Counter should be one, that means we didn't look further than needed", 1, counter)
  }

  @Test def groupedIteratorIsLazyWhenPadded(): Unit = {
    var counter = 0
    def it = new Iterator[Int] { var i = 0 ; def hasNext = { counter = i; true } ; def next = { i += 1; i } }
    val slidingIt = it sliding 2 withPadding -1
    slidingIt.next
    assertEquals("Counter should be one, that means we didn't look further than needed", 1, counter)
  }

  // SI-8552
  @Test def indexOfShouldWorkForTwoParams(): Unit = {
    val testList = List(1,2,3)
    assertEquals(1, testList.iterator.indexOf(2, 0))
    assertEquals(testList.indexOf(3, 1), testList.iterator.indexOf(3, 1))
    assertEquals(-1, Iterator(5 -> 0).indexOf(5,0))
    assertEquals(-1, Iterator(5 -> 0, (9, 2)).indexOf(9, 1))
    assertEquals(1, Iterator(5 -> 0, (9, 2), 0 -> 3).indexOf(9 -> 2))
  }
}
