package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert.assertEquals

import scala.tools.testkit.AssertUtil.{assertFails, assertNotReachable, assertThrows}
import scala.util.Random
import scala.util.chaining._

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

@RunWith(classOf[JUnit4])
/* Test for scala/bug#7568  */
class PriorityQueueTest {
  val elements = List.fill(1000)(Random.nextInt(Int.MaxValue))
  def priorityQueue = PriorityQueue.empty[Int].tap(_.enqueue(elements:_*))

  @deprecated("Tests deprecated API", since="2.13")
  @Test
  def orderedCompanion(): Unit = {
    val pq = PriorityQueue.empty[Int]
    assert(pq.orderedCompanion == PriorityQueue)
  }

  @Test
  def orderingReverseReverse(): Unit = {
    val pq = PriorityQueue.empty[Nothing]((_,_) => 42)
    assert(pq.ord eq pq.reverse.reverse.ord)
  }

  @Test /* Regression for https://github.com/scala/bug/issues/11439 */
  def emptyMapInPlace(): Unit = {
    val pq = PriorityQueue.empty[String].mapInPlace(_.toUpperCase) // used to crash because of the weird resarr implementation
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
    assert(List(1,2,3,4,5).contains(PriorityQueue[Int](1,2,3,4,5).last))
    assertThrows[NoSuchElementException](PriorityQueue.empty[Int].last)
  }

  @Test
  def copyToArray(): Unit = {

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
  @Test def `pq does not hold onto elements`: Unit = {
    implicit val `order anything, it's on me`: Ordering[Any] = implicitly[Ordering[String]].on(_.toString)
    val thing = new Object
    val pq = PriorityQueue.empty[Any]
    assertNotReachable(thing, pq) {
      pq.addOne(thing)
      assertEquals(thing, pq.dequeue())
    }
    assertNotReachable(thing, pq) {
      pq.addOne(thing)
      pq.clear()
    }
    // sanity check
    assertFails(_.contains("held reference")) {
      assertNotReachable(thing, pq) {
        pq.addOne(thing)
      }
    }
  }
  @Test def `pq may be rebuilt after mutation`: Unit = {
    case class Thing(var s: String)
    implicit val `order by s`: Ordering[Thing] = implicitly[Ordering[String]].on(_.s)
    val pq = PriorityQueue.empty[Thing]
    val thingOne = Thing("abc")
    val thingTwo = Thing("def")
    pq.addOne(thingOne)
    pq.addOne(thingTwo)
    assertEquals(thingTwo, pq.clone.dequeue())
    thingTwo.s = "aaa"
    val newly = PriorityQueue.from(pq)
    assertEquals(thingOne, newly.dequeue())
    assertEquals(thingTwo, pq.clone.dequeue())
  }
}
