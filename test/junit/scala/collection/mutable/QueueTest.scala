package scala.collection.mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.collection.mutable

@RunWith(classOf[JUnit4])
class QueueTest {

  @Test
  def reversingReturnsAQueue(): Unit = {
    val q1 = Queue(1, 2, 3)
    val q2: Queue[Int] = q1.reverse
    assertEquals("Queue", q2.collectionClassName)
  }

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

  @Test
  def copyToArrayOutOfBounds(): Unit = {
    val target = Array[Int]()
    assertEquals(0, mutable.Queue(1, 2).copyToArray(target, 1, 0))
  }

  @Test
  def insertsWhenResizeIsNeeded(): Unit = {
    val q = Queue.from(Array.range(0, 15))
    q.insert(1, -1)
    assertEquals(Queue(0, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14), q)
  }

  @Test
  def sliding(): Unit = ArrayDequeTest.genericSlidingTest(Queue, "Queue")
}
