package scala.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test


import org.junit.Assert.assertEquals

@RunWith(classOf[JUnit4])
/* Tests for collection.immutable.Queue  */
class QueueTest {
  val emptyQueue = Queue.empty[Int]
  val oneAdded = emptyQueue.enqueue(1)
  val threeAdded = emptyQueue.enqueue(1 to 3)



  @Test
  def dequeueOptionOnEmpty(): Unit = {
    assertEquals(emptyQueue.dequeueOption, None)
  }

  @Test
  def dequeueOptionOneAdded(): Unit = {
    assertEquals(oneAdded.dequeueOption, Some((1,emptyQueue)))
  }

  @Test
  def dequeueOptionThreeAdded(): Unit = {
    assertEquals(threeAdded.dequeueOption, Some((1,Queue(2 to 3:_*))))
  }

  @Test
  def copyToArrayOutOfBounds: Unit = {
    val target = Array[Int]()
    assertEquals(0, Queue(1,2).copyToArray(target, 1, 0))
  }

}
