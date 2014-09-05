
package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil._

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
    assertSameElements(1 to 4, Iterator.from(0).take(5).drop(1).toList)
    assertSameElements(3 to 4, Iterator.from(0).take(5).drop(3).toList)
    assertSameElements(Seq.empty, Iterator.from(0).take(5).drop(5).toList)
    assertSameElements(Seq.empty, Iterator.from(0).take(5).drop(10).toList)
    assertSameElements(0 to 4, Iterator.from(0).take(5).drop(0).toList)
    assertSameElements(0 to 4, Iterator.from(0).take(5).drop(-1).toList)
    assertSameElements(2 to 8 by 2, (Iterator from 0 take 5 drop 1 map (2 * _)).toList)
    assertSameElements(2 to 8 by 2, (Iterator from 0 take 5 map (2 * _) drop 1).toList)
    assertSameElements(3 to 4, (Iterator from 0 take 5 drop 1 drop 2).toList)
    assertSameElements(3 to 4, (Iterator from 0 take 5 drop 2 drop 1).toList)
  }

  @Test def sliceIsChainable(): Unit = {
    assertSameElements(3 to 6, Iterator from 0 slice (3, 7))
    assertSameElements(Seq.empty, Iterator from 0 slice (3, 3))
    assertSameElements(0 to 2, Iterator from 0 slice (-1, 3))
    assertSameElements(Seq.empty, Iterator from 0 slice (3, -1))
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
    assertEquals(-1, it1.next() + it2.next())
  }
  @Test def range(): Unit = {
    val xs1 = Iterator.range(0, 10,  2)
    val xs2 = Iterator.range(0, 10, -2)
    val xs3 = Iterator.range(10, 0, -2)
    val xs4 = Iterator.range(10, 0,  2)
    val xs5 = Iterator.range(0, 10, 11)
    assertEquals(11, xs1.size + xs2.size + xs3.size + xs4.size + xs5.size)
  }
  @Test def range2(): Unit = {
    val r1start = 0
    val r1end = 10
    val r1step = 1
    val r1 = Iterator.range(r1start, r1end, r1step)
    val r2 = Iterator.range(r1start, r1end, r1step + 1)
    val r3 = Iterator.range(r1end, r1start, -r1step)
    val r4 = Iterator.range(0, 10, 11)
    // 10 + 5 + 10 + 1
    assertEquals(10 + 5 + 10 + 1, r1.size + r2.size + r3.size + r4.size)
  }
  @Test def range3(): Unit = {
    def trues(xs: List[Boolean]) = xs.foldLeft(0)((a, b) => if (b) a+1 else a)
    val r1 = Iterator.range(0, 10)
    val xs1 = List(r1 contains 5, r1 contains 6)
    val r2a = Iterator.range(0, 10, 2)
    val r2b = Iterator.range(0, 10, 2)
    val xs2 = List(r2a contains 5, r2b contains 6)
    val r3 = Iterator.range(0, 10, 11)
    val xs3 = List(r3 contains 5, r3 contains 6)
    // 2 + 1 + 0
    assertEquals(2 + 1 + 0, trues(xs1) + trues(xs2) + trues(xs3))
  }
  @Test def take(): Unit = {
    val it1 = Iterator.from(0)
    val xs1 = it1 take 10
    assertEquals(10, xs1.size)
  }
  @Test def foreach(): Unit = {
    val it1 = Iterator.from(0) take 20
    var n = 0
    it1 foreach { n += _ }
    assertEquals(190, n)
  }
  /* ???
  @Test def forall(): Unit = {
    val it1 = Iterator.from(0)
    val it2 = Iterator.from(1)
    0
  }
  */
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
    assertEquals(3, List(1, 2, 3, 4, 5).indexOf(4))
    assertEquals(-1, List(1, 2, 3, 4, 5).indexOf(16))
  }
  @Test def indexWhere(): Unit = {
    assertEquals(3, List(1, 2, 3, 4, 5).indexWhere { x: Int => x >= 4 })
    assertEquals(-1, List(1, 2, 3, 4, 5).indexWhere { x: Int => x >= 16 })
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
}
