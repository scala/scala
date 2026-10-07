package scala.collection.mutable

import org.junit.Test
import org.junit.Assert._

import scala.annotation.nowarn
import scala.collection.SeqFactory
import scala.collection.immutable.List
import scala.tools.nsc.`strip margin`
import scala.tools.testkit.AssertUtil.assertThrows

class ArrayDequeTest {

  @Test
  def apply: Unit = {
    val buffer = ArrayDeque.empty[Int]
    val standard = ArrayBuffer.empty[Int]

    def apply[U](f: Buffer[Int] => U) = {
      //println(s"Before: [buffer1=${buffer}; buffer2=${buffer2}]")
      assertEquals(f(standard), f(buffer))
      assertEquals(standard, buffer)
      assertEquals(standard.reverse, buffer.reverse)
    }

    apply(_.+=(1, 2, 3, 4, 5)): @nowarn("cat=deprecation")
    apply(_.prepend(6).prepend(7).prepend(8))
    apply(_.dropInPlace(2))
    apply(_.dropRightInPlace(3))
    apply(_.insert(2, -3))
    apply(_.insertAll(0, collection.Seq(9, 10, 11)))
    apply(_.insertAll(1, collection.Seq(12, 13)))
    apply(_.insertAll(0, collection.Seq(23, 24)))
    apply(_ ++= Seq(25, 26))
    apply(_.insertAll(3, collection.IndexedSeq(18, 33)))
    apply(_.remove(2))
    apply(_.prependAll(collection.Seq(14, 15, 16, 17)))
    apply(_.remove(1, 5))
    apply(_.prependAll(Seq.tabulate(100)(identity)))
    apply(b => b.insertAll(b.length - 5, collection.Seq.tabulate(10)(identity)))
    buffer.trimToSize()
    apply(_.addAll(collection.Seq.tabulate(100)(identity)))
    apply(_.addAll(collection.Iterator.tabulate(100)(identity)))
    apply(_.addAll(collection.immutable.Vector.tabulate(10)(identity)))

    (-100 to 100) foreach {i =>
      assertEquals(standard.splitAt(i), buffer.splitAt(i))
    }

    for {
      i <- -100 to 100
      j <- -100 to 100
    } {
      assertEquals(standard.slice(i, j), buffer.slice(i, j))
      if (i > 0 && j > 0) assertEquals(List.from(standard.sliding(i, j)), List.from(buffer.sliding(i, j)))
    }
  }

  @Test
  def queueBounds(): Unit = {
    import scala.collection.mutable.Queue

    val xs = Queue.empty[Int]
    assertEquals("Queue()", xs.toString)

    val a = xs.toArray
    assertEquals(0, a.length)

    xs.insert(0, 0)
    assertEquals(Queue(0), xs)
  }

  @Test
  def copyToArrayOutOfBounds(): Unit = {
    val target = Array[Int]()
    assertEquals(0, collection.mutable.ArrayDeque(1, 2).copyToArray(target, 1, 0))
  }

  @Test
  def insertsWhenResizeIsNeeded(): Unit = {
    val arrayDeque = ArrayDeque.from(Array.range(0, 15))
    arrayDeque.insert(1, -1)
    assertEquals(ArrayDeque(0, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14), arrayDeque)
  }

  @Test
  def insertAll(): Unit = {
    var a = ArrayDeque(0, 1)
    a.insertAll(1, Seq(2))
    assertEquals(ArrayDeque(0, 2, 1), a)
    a = ArrayDeque(0, 1)
    a.insertAll(2, Seq(2))
    assertEquals(ArrayDeque(0, 1, 2), a)
    var q = Queue(0, 1)
    q.patchInPlace(1, Seq(2), 0)
    assertEquals(Queue(0, 2, 1), q)
    q = Queue(0, 1)
    q.patchInPlace(2, Seq(2), 0)
    assertEquals(Queue(0, 1, 2), q)
  }

  @Test
  def sliding(): Unit = ArrayDequeTest.genericSlidingTest(ArrayDeque, "ArrayDeque")


  class PeekingArrayDeque[C] extends ArrayDeque[C] {
    def capacity = array.length
  }

  @Test
  def trimToSize(): Unit = {
    val a = new PeekingArrayDeque().addAll(0 to 255)

    a.trimToSize()  // Can't shrink
    assertEquals(a.capacity, 512)
    a.remove(0)     // No reallocation because array isn't empty enough
    assertEquals(a.capacity, 512)
    a.trimToSize()  // Shrink to 256
    assertEquals(a.capacity, 256)
  }

  @Test def `head of empty throws NoSuchElement`: Unit =
    assertThrows[NoSuchElementException](ArrayDeque.empty[Int].head, _.endsWith("head of empty ArrayDeque"))

  @Test def `last of empty throws NoSuchElement`: Unit =
    assertThrows[NoSuchElementException](ArrayDeque.empty[Int].last, _.endsWith("last of empty ArrayDeque"))

  @Test def `sliding throws on shrink`: Unit = {
    val sut = ArrayDeque.from(1 to 10)
    val it = sut.sliding(size = 2, step = 1)
    assertTrue(it.hasNext)
    assertEquals(2, it.next().size)
    sut.clear()
    assertThrows[java.util.ConcurrentModificationException](it.hasNext)
  }

  @Test def `sliding throws on grow`: Unit = {
    val sut = ArrayDeque.from(1 to 10)
    val it = sut.sliding(size = 2, step = 1)
    assertTrue(it.hasNext)
    assertEquals(2, it.next().size)
    sut.addOne(100)
    assertThrows[java.util.ConcurrentModificationException](it.hasNext)
  }
}

object ArrayDequeTest {

  // tests scala/bug#11047
  def genericSlidingTest(factory: SeqFactory[ArrayDeque], collectionName: String): Unit = {
    for (i <- 0 to 40; j <- 1 to 40; k <- 1 to 40) {
      val range = 0 until i
      val other = factory.from(range)
      val iterableSliding = range.sliding(j, k).to(Seq)
      val otherSliding = other.sliding(j, k).to(Seq)
      assert(iterableSliding == otherSliding,
        sm"""Iterable.from($range)).sliding($j,$k) differs from $collectionName.from($range)).sliding($j,$k)
            |Iterable yielded: $iterableSliding
            |$collectionName yielded: $otherSliding
            |"""
      )
    }

    // scala/bug#11440
    assertEquals(0, factory.empty[Int].sliding(1).size)

    // no general mutation check
    locally {
      val sut = factory.from(1 to 2)
      val it = sut.sliding(size = 2, step = 1)
      sut(1) = 100
      assertTrue(it.hasNext)
      assertEquals(1, it.size)
    }
  }
}
