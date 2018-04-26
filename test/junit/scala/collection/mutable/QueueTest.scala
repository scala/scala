package scala.collection.mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.collection.IterableFactory

@RunWith(classOf[JUnit4])
class QueueTest {

  @Test
  def reversingReturnsAQueue(): Unit = {
    val q1 = Queue(1, 2, 3)
    val q2: Queue[Int] = q1.reverse
    assertEquals("Queue", q2.className)
  }
}
