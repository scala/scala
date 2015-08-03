package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import scala.collection.mutable
import java.io.{ObjectInputStream, ByteArrayInputStream, ByteArrayOutputStream, ObjectOutputStream}

@RunWith(classOf[JUnit4])
/* Test for SI-7568  */
class PriorityQueueTest {
  val priorityQueue = new mutable.PriorityQueue[Int]()
  val elements = List.fill(1000)(scala.util.Random.nextInt(Int.MaxValue))
  priorityQueue.enqueue(elements :_*)

  @Test
  def canSerialize() {
    val outputStream = new ByteArrayOutputStream()
    new ObjectOutputStream(outputStream).writeObject(priorityQueue)
  }

  @Test
  def stringRepresentationShowHeadIfExists(): Unit = {
    val pq = new mutable.PriorityQueue[Int]()
    pq += 2
    pq += 10
    pq += 3

    assert(pq.toString == "PriorityQueue(head=Some(10))", pq.toString)
  }

  @Test
  def stringRepresentationShowNoneIfExists(): Unit = {
    val pq = new mutable.PriorityQueue[Int]()

    assert(pq.toString == "PriorityQueue(head=None)", pq.toString)
  }

  @Test
  def maintainsStateWhenDeserialized() {
    val outputStream = new ByteArrayOutputStream()
    new ObjectOutputStream(outputStream).writeObject(priorityQueue)
    val bytes = outputStream.toByteArray

    val objectInputStream = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val deserializedPriorityQueue = objectInputStream.readObject().asInstanceOf[PriorityQueue[Int]]
    assert(deserializedPriorityQueue.dequeueAll == elements.sorted.reverse)
  }
}
