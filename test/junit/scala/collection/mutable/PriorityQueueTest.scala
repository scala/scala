package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

import scala.collection.mutable
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import scala.tools.testkit.AssertUtil.assertThrows

@RunWith(classOf[JUnit4])
/* Test for scala/bug#7568  */
class PriorityQueueTest {
  val priorityQueue = new mutable.PriorityQueue[Int]()
  val elements = List.fill(1000)(scala.util.Random.nextInt(Int.MaxValue))
  priorityQueue.enqueue(elements :_*)

  @Test
  def orderedCompanion(): Unit = {
    val pq = new mutable.PriorityQueue[Int]()
    assert(pq.orderedCompanion == PriorityQueue)
  }

  @Test
  def orderingReverseReverse(): Unit = {
    val pq = new mutable.PriorityQueue[Nothing]()((_,_)=>42)
    assert(pq.ord eq pq.reverse.reverse.ord)
  }

  @Test /* Regression for https://github.com/scala/bug/issues/11439 */
  def emptyMapInPlace(): Unit = {
    val pq = mutable.PriorityQueue.empty[String].mapInPlace(_.toUpperCase) // used to crash because of the weird resarr implementation
    assert(pq.isEmpty)
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
  @Test
  def lastOfEmptyThrowsException(): Unit = {
    assert(List(1,2,3,4,5).contains(collection.mutable.PriorityQueue[Int](1,2,3,4,5).last))
    assertThrows[NoSuchElementException](collection.mutable.PriorityQueue[Int]().last)
  }

  @Test
  def copyToArray: Unit = {

    for {
      size <- Seq(0, 1, 2, 5, 10, 65, 10000)
      pq = (0 until size).to(PriorityQueue)
      dstSize <- Seq(0, 1, 2, 5, 10, 65, 10000, 10001)
      strt <- Seq(0, 1, 2, 5, 10, 65, 10000, 10001)
      len <- Seq(0, 1, 2, 5, 10, 65, 10000, 10001)
    } {
      def dst = new Array[Int](dstSize)

      val dst0 = dst
      val dst1 = dst
      pq.copyToArray(dst0, strt, len)
      pq.iterator.copyToArray(dst1, strt, len)

      val v0 = dst0.toVector
      val v1 = dst1.toVector

      assert(v0 == v1,
        s"""Failed on:
           |  size: $size
           |  destinationSize: $dstSize
           |  start: $strt
           |  len: $len
           |  from PriorityQueue: $v0
           |  from iterator: $v1
         """.stripMargin
      )
    }
  }
}
