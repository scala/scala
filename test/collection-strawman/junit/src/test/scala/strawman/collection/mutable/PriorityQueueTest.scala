package strawman.collection.mutable

import strawman.collection.immutable.List
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
/* Test for scala/bug#7568  */
class PriorityQueueTest {
  val priorityQueue = new PriorityQueue[Int]()
  val elements = List.fill(1000)(scala.util.Random.nextInt(Int.MaxValue))
  priorityQueue.enqueue(elements.toClassic :_*)

  @Test
  def orderingReverseReverse(): Unit = {
    val pq = new PriorityQueue[Nothing]()((_,_)=>42)
    assert(pq.ord eq pq.reverse.reverse.ord)
  }
  
  @Test
  def canSerialize(): Unit = {
    val outputStream = new ByteArrayOutputStream()
    new ObjectOutputStream(outputStream).writeObject(priorityQueue)
  }

  @Test
  def maintainsStateWhenDeserialized(): Unit = {
    val outputStream = new ByteArrayOutputStream()
    new ObjectOutputStream(outputStream).writeObject(priorityQueue)
    val bytes = outputStream.toByteArray

    val objectInputStream = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val deserializedPriorityQueue = objectInputStream.readObject().asInstanceOf[PriorityQueue[Int]]
    //correct sequencing is also tested here:
    assert(deserializedPriorityQueue.dequeueAll == elements.sorted.reverse)
  }
}
