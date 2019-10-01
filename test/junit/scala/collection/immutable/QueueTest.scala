package scala.collection.immutable

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
/* Tests for collection.immutable.Queue  */
class QueueTest {

  private val emptyQueue = Queue.empty[Int]

  @Test
  def dequeueOptionOnEmpty(): Unit = {
    assertEquals(None, emptyQueue.dequeueOption)
  }

  @Test
  def dequeueOptionOneAdded(): Unit = {
    val oneAdded = emptyQueue.enqueue(1)
    assertEquals(Some((1,emptyQueue)), oneAdded.dequeueOption)
  }

  @Test
  def dequeueOptionThreeAdded(): Unit = {
    val threeAdded = emptyQueue.enqueueAll(1 to 3)
    assertEquals(
      Some((1,Queue(2 to 3:_*))),
      threeAdded.dequeueOption,
    )
  }

  @Test
  def copyToArrayOutOfBounds: Unit = {
    val target = Array[Int]()
    assertEquals(0, Queue(1,2).copyToArray(target, 1, 0))
  }

  @Test def `iterator works as lazy concat`(): Unit = {
    val three = Queue(1, 2, 3)
    val six   = three.enqueueAll(4 to 6)
    val it    = six.iterator
    val more  = six.enqueue(7)
    assertEquals(1, it.next())   // no list.reverse here
    assertEquals(5, it.size)
    assertEquals(7, more.iterator.size)
  }

}
