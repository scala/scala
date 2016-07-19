
package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil._

import Seq.empty

@RunWith(classOf[JUnit4])
class IteratorTest {

  @Test def groupedIteratorShouldNotAskForUnneededElement(): Unit = {
    var counter = 0
    val it = new Iterator[Int] { var i = 0 ; def hasNext = { counter = i; true } ; def next = { i += 1; i } }
    val slidingIt = it sliding 2
    slidingIt.next
    assertEquals("Counter should be one, that means we didn't look further than needed", 1, counter)
  }

  @Test def groupedIteratorIsLazyWhenPadded(): Unit = {
    var counter = 0
    def it = new Iterator[Int] { var i = 0 ; def hasNext = { counter = i; true } ; def next = { i += 1; i } }
    val slidingIt = it sliding 2 withPadding -1
    slidingIt.next
    assertEquals("Counter should be one, that means we didn't look further than needed", 1, counter)
  }

  @Test def dropDoesNotGrowStack(): Unit = {
    def it = new Iterator[Throwable] { def hasNext = true ; def next = new Throwable }

    assertEquals(it.drop(1).next.getStackTrace.length, it.drop(1).drop(1).next.getStackTrace.length)
  }

  @Test def dropIsChainable(): Unit = {
    assertSameElements(1 to 4, Iterator from 0 take 5 drop 1)
    assertSameElements(3 to 4, Iterator from 0 take 5 drop 3)
    assertSameElements(empty,  Iterator from 0 take 5 drop 5)
    assertSameElements(empty,  Iterator from 0 take 5 drop 10)
    assertSameElements(0 to 4, Iterator from 0 take 5 drop 0)
    assertSameElements(0 to 4, Iterator from 0 take 5 drop -1)
    assertSameElements(2 to 8 by 2, Iterator from 0 take 5 drop 1 map (2 * _))
    assertSameElements(2 to 8 by 2, Iterator from 0 take 5 map (2 * _) drop 1)
    assertSameElements(3 to 4, Iterator from 0 take 5 drop 1 drop 2)
    assertSameElements(3 to 4, Iterator from 0 take 5 drop 2 drop 1)
  }

  @Test def sliceIsChainable(): Unit = {
    assertSameElements(3 to 6, Iterator from 0 slice (3, 7))
    assertSameElements(empty,  Iterator from 0 slice (3, 3))
    assertSameElements(0 to 2, Iterator from 0 slice (-1, 3))
    assertSameElements(empty,  Iterator from 0 slice (3, -1))
    assertSameElements(6 to 12 by 2, Iterator from 0 slice (3, 7) map (2 * _))
    assertSameElements(6 to 12 by 2, Iterator from 0 map (2 * _) slice (3, 7))
    assertSameElements(4 to 6, Iterator from 0 slice (3, 7) drop 1)
    assertSameElements(4 to 7, Iterator from 0 drop 1 slice (3, 7))
    assertSameElements(4 to 5, Iterator from 0 slice (3, 7) slice (1, 3))
    assertSameElements(4 to 6, Iterator from 0 slice (3, 7) slice (1, 10))
  }

  // test/files/run/iterator-concat.scala
  @Test def concatIsStackFriendly(): Unit = {
    // Create `size` Function0s, each of which evaluates to an Iterator
    // which produces 1. Then fold them over ++ to get a single iterator,
    // which should sum to "size".
    def mk(size: Int): Iterator[Int] = {
      //val closures = (1 to size).toList.map(x => (() => Iterator(1)))
      //closures.foldLeft(Iterator.empty: Iterator[Int])((res, f) => res ++ f())
      List.fill(size)(() => Iterator(1)).foldLeft(Iterator.empty: Iterator[Int])((res, f) => res ++ f())
    }
    assertEquals(100,    mk(100).sum)
    assertEquals(1000,   mk(1000).sum)
    assertEquals(10000,  mk(10000).sum)
    assertEquals(100000, mk(100000).sum)
  }

  @Test def from(): Unit = {
    val it1 = Iterator.from(-1)
    val it2 = Iterator.from(0, -1)
    assertEquals(-1, it1.next())
    assertEquals(0,  it2.next())
  }
  @Test def range(): Unit = {
    assertEquals(5, Iterator.range(0, 10, 2).size)
    assertEquals(0, Iterator.range(0, 10, -2).size)
    assertEquals(5, Iterator.range(10, 0, -2).size)
    assertEquals(0, Iterator.range(10, 0, 2).size)
    assertEquals(1, Iterator.range(0, 10, 11).size)
    assertEquals(10, Iterator.range(0, 10, 1).size)
    assertEquals(10, Iterator.range(10, 0, -1).size)
  }
  @Test def range3(): Unit = {
    val r1 = Iterator.range(0, 10)
    assertTrue(r1 contains 5)
    assertTrue(r1 contains 6)
    assertFalse(r1 contains 4)
    val r2a = Iterator.range(0, 10, 2)
    assertFalse(r2a contains 5)
    val r2b = Iterator.range(0, 10, 2)
    assertTrue(r2b contains 6)
    val r3 = Iterator.range(0, 10, 11)
    assertFalse(r3 contains 5)
    assertTrue(r3.isEmpty)
  }
  @Test def take(): Unit = {
    assertEquals(10, (Iterator from 0 take 10).size)
  }
  @Test def foreach(): Unit = {
    val it1 = Iterator.from(0) take 20
    var n = 0
    it1 foreach { n += _ }
    assertEquals(190, n)
  }
  // ticket #429
  @Test def fromArray(): Unit = {
    val a = List(1, 2, 3, 4).toArray
    var xs0 = a.iterator.toList;
    var xs1 = a.slice(0, 1).iterator
    var xs2 = a.slice(0, 2).iterator
    var xs3 = a.slice(0, 3).iterator
    var xs4 = a.slice(0, 4).iterator
    assertEquals(14, xs0.size + xs1.size + xs2.size + xs3.size + xs4.size)
  }
  @Test def toSeq(): Unit = {
    assertEquals("1x2x3x4x5", List(1, 2, 3, 4, 5).iterator.mkString("x"))
  }
  @Test def indexOf(): Unit = {
    assertEquals(3, List(1, 2, 3, 4, 5).iterator.indexOf(4))
    assertEquals(-1, List(1, 2, 3, 4, 5).iterator.indexOf(16))
  }
  @Test def indexWhere(): Unit = {
    assertEquals(3, List(1, 2, 3, 4, 5).iterator.indexWhere { x: Int => x >= 4 })
    assertEquals(-1, List(1, 2, 3, 4, 5).iterator.indexWhere { x: Int => x >= 16 })
  }
  @Test def indexOfFrom(): Unit = {
    assertEquals(1, List(1, 2, 3, 4, 5).iterator.indexOf(2, 0))
    assertEquals(1, List(1, 2, 3, 4, 5).iterator.indexOf(2, 1))
    assertEquals(-1, List(1, 2, 3, 4, 5).iterator.indexOf(2, 2))
    assertEquals(4, List(1, 2, 3, 2, 1).iterator.indexOf(1, 1))
    assertEquals(1, List(1, 2, 3, 2, 1).iterator.indexOf(2, 1))
  }
  @Test def indexWhereFrom(): Unit = {
    assertEquals(1, List(1, 2, 3, 4, 5).iterator.indexWhere(_ == 2, 0))
    assertEquals(1, List(1, 2, 3, 4, 5).iterator.indexWhere(_ == 2, 1))
    assertEquals(-1, List(1, 2, 3, 4, 5).iterator.indexWhere(_ == 2, 2))
    assertEquals(4, List(1, 2, 3, 2, 1).iterator.indexWhere(_ < 2, 1))
    assertEquals(1, List(1, 2, 3, 2, 1).iterator.indexWhere(_ <= 2, 1))
  }
  // iterator-iterate-lazy.scala
  // was java.lang.UnsupportedOperationException: tail of empty list
  @Test def iterateIsSufficientlyLazy(): Unit = {
    //Iterator.iterate((1 to 5).toList)(_.tail).takeWhile(_.nonEmpty).toList  // suffices
    Iterator.iterate((1 to 5).toList)(_.tail).takeWhile(_.nonEmpty).map(_.head).toList
  }
  // SI-3516
  @Test def toStreamIsSufficientlyLazy(): Unit = {
    val results = collection.mutable.ListBuffer.empty[Int]
    def mkIterator = (1 to 5).iterator map (x => { results += x ; x })
    def mkInfinite = Iterator continually { results += 1 ; 1 }

    // Stream is strict in its head so we should see 1 from each of them.
    val s1 = mkIterator.toStream
    val s2 = mkInfinite.toStream
    // back and forth without slipping into nontermination.
    results += (Stream from 1).toIterator.drop(10).toStream.drop(10).toIterator.next()
    assertSameElements(List(1,1,21), results)
  }
  // SI-8552
  @Test def indexOfShouldWorkForTwoParams(): Unit = {
    assertEquals(1, List(1, 2, 3).iterator.indexOf(2, 0))
    assertEquals(-1, List(5 -> 0).iterator.indexOf(5, 0))
    assertEquals(0, List(5 -> 0).iterator.indexOf((5, 0)))
    assertEquals(-1, List(5 -> 0, 9 -> 2, 0 -> 3).iterator.indexOf(9, 2))
    assertEquals(1, List(5 -> 0, 9 -> 2, 0 -> 3).iterator.indexOf(9 -> 2))
  }
  // SI-9332
  @Test def spanExhaustsLeadingIterator(): Unit = {
    def it = Iterator.iterate(0)(_ + 1).take(6)
    val (x, y) = it.span(_ != 1)
    val z = x.toList
    assertEquals(1, z.size)
    assertFalse(x.hasNext)
    assertEquals(1, y.next)
    assertFalse(x.hasNext)   // was true, after advancing underlying iterator
  }
  // SI-9623
  @Test def noExcessiveHasNextInJoinIterator: Unit = {
    var counter = 0
    val exp = List(1,2,3,1,2,3)
    def it: Iterator[Int] = new Iterator[Int] {
      val parent = List(1,2,3).iterator
      def next(): Int = parent.next
      def hasNext: Boolean = { counter += 1; parent.hasNext }
    }
    // Iterate separately
    val res = new mutable.ArrayBuffer[Int]
    it.foreach(res += _)
    it.foreach(res += _)
    assertSameElements(exp, res)
    assertEquals(8, counter)
    // JoinIterator
    counter = 0
    res.clear
    (it ++ it).foreach(res += _)
    assertSameElements(exp, res)
    assertEquals(8, counter) // was 17
    // ConcatIterator
    counter = 0
    res.clear
    (Iterator.empty ++ it ++ it).foreach(res += _)
    assertSameElements(exp, res)
    assertEquals(8, counter) // was 14
  }
  // SI-9691
  @Test def bufferedHeadOptionReturnsValueWithHeadOrNone(): Unit = {
    // Checks BufferedIterator returns Some(value) when there is a value
    val validHeadOption = List(1,2,3).iterator.buffered.headOption
    assertEquals(Some(1), validHeadOption)
    // Checks BufferedIterator returns None when there is no value
    val invalidHeadOption = List(1,2,3).iterator.drop(10).buffered.headOption
    assertEquals(None: Option[Int], invalidHeadOption)
    // Checks BufferedIterator returns Some(value) in the last position with a value
    val validHeadOptionAtTail = List(1,2,3).iterator.drop(2).buffered.headOption
    assertEquals(Some(3), validHeadOptionAtTail)
    // Checks BufferedIterator returns None at the first position without a value
    val invalidHeadOptionOnePastTail = List(1,2,3).iterator.drop(3).buffered.headOption
    assertEquals(None, invalidHeadOptionOnePastTail)
    // Checks BufferedIterator returns Some(null) if the next value is null.
    val nullHandingList = List(null, "yellow").iterator.buffered.headOption
    assertEquals(Some(null), nullHandingList)
    // Checks that BufferedIterator is idempotent. That the head is not
    // changed by its invocation, nor the headOption by the next call to head.
    val it = List(1,2,3).iterator.buffered
    val v1 = it.head
    val v2 = it.headOption
    val v3 = it.head
    val v4 = it.headOption
    assertEquals(v1, v3)
    assertEquals(v2, v4)
    assertEquals(Some(v1), v2)
  }
}
