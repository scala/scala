package scala.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
/* Tests for collection.immutable.Queue  */
class QueueTest {
  val emptyQueue = Queue.empty[Int]
  val oneAdded = emptyQueue.enqueue(1)
  val threeAdded = emptyQueue.enqueue(1 to 3)

  @Test
  def dequeueOptionOnEmpty() {
    assert( emptyQueue.dequeueOption == None )
  }

  @Test
  def dequeueOptionOneAdded() {
    assert( oneAdded.dequeueOption == Some((1,emptyQueue)) )
  }

  @Test
  def dequeueOptionThreeAdded() {
    assert( threeAdded.dequeueOption == Some((1,Queue(2 to 3:_*))) )
  }
}
