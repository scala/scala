package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

@RunWith(classOf[JUnit4])
class QueueTest {
  @Test
  def t9005(): Unit = {
    val q = Queue(1, 2, 3)
    assertEquals(q, List(1, 2, 3))
    val r = q.tail
    assertEquals(r, List(2, 3))
    assertEquals(q.enqueue(4), List(1, 2, 3, 4))
    assertEquals(r, List(2, 3))
    val s = q.drop(1)
    assertEquals(s, List(2, 3, 4))
    assertEquals(q.enqueue(5), List(1, 2, 3, 4, 5))
    assertEquals(s, List(2, 3, 4))
  }
}
