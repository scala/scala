package strawman
package collection.mutable

import strawman.collection.immutable.List

import org.junit.Test
import org.junit.Assert._

class ArrayDequeTest {

  @Test
  def apply() = {
    val buffer = ArrayDeque.empty[Int]
    val buffer2 = ArrayBuffer.empty[Int]

    def apply[U](f: Buffer[Int] => U) = {
      //println(s"Before: [buffer1=${buffer}; buffer2=${buffer2}]")
      assert(f(buffer) == f(buffer2))
      assert(buffer == buffer2)
      assert(buffer.reverse == buffer2.reverse)
    }

    apply(_ += (1, 2, 3, 4, 5))
    apply(_.prepend(6).prepend(7).prepend(8))
    apply(_.trimStart(2))
    apply(_.trimEnd(3))
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
      assert(buffer.splitAt(i) == buffer2.splitAt(i))
    }

    for {
      i <- -100 to 100
      j <- -100 to 100
    } {
      assert(buffer.slice(i, j) == buffer2.slice(i, j))
      if (i > 0 && j > 0) assert(List.from(buffer.sliding(i, j)) == List.from(buffer2.sliding(i, j)))
    }
  }

  @Test
  def queueBounds: Unit = {
    import strawman.collection.mutable.Queue

    val xs = Queue.empty[Int]
    assertEquals("Queue()", xs.toString)

    val a = xs.toArray
    assertEquals(0, a.length)

    xs.insert(0, 0)
    assertEquals(Queue(0), xs)
  }
}
