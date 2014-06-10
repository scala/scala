
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
}
