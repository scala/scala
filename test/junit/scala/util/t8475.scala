
package scala.util
package test

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class GroupedIteratorTest {

  @Test
  def shouldNotAskForUnneededElement(): Unit = {
    var counter = 0
    val it = new Iterator[Int] { var i = 0 ; def hasNext = { counter = i; true } ; def next = { i += 1; i } }
    val slidingIt = it sliding 2
    slidingIt.next
    assert(counter == 1)
  }
}
