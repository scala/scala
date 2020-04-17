package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.AssertUtil._

import Seq.empty

@RunWith(classOf[JUnit4])
class IteratorTest {

  @Test def groupedIteratorShouldNotAskForUnneededElement(): Unit = {
    var counter = 0
    val it = new Iterator[Int] { var i = 0 ; def hasNext = { counter = i; true } ; def next() = { i += 1; i } }
    val slidingIt = it sliding 2
    slidingIt.next
    assertEquals("Counter should be one, that means we didn't look further than needed", 1, counter)
  }

  @Test def groupedIteratorIsLazyWhenPadded(): Unit = {
    var counter = 0
    def it = new Iterator[Int] { var i = 0 ; def hasNext = { counter = i; true } ; def next() = { i += 1; i } }
    val slidingIt = it sliding 2 withPadding -1
    slidingIt.next
    assertEquals("Counter should be one, that means we didn't look further than needed", 1, counter)
  }

  @Test def dropDoesNotGrowStack(): Unit = {
    def it = new Iterator[Throwable] { def hasNext = true ; def next() = new Throwable }

    assertEquals(it.drop(1).next().getStackTrace.length, it.drop(1).drop(1).next().getStackTrace.length)
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
  @Test def rangeOverflow(): Unit = {
    val step = 100000000
    val numExpectedSamples = 22
    def createIterator = Iterator.range(0, Int.MaxValue, step)
    assertEquals(numExpectedSamples, createIterator.count(_ => true))
    assertEquals(0, createIterator.min)
    assertEquals((numExpectedSamples - 1) * step, createIterator.max)
  }
  @Test def rangeOverflow2() : Unit = {
    val step = (Int.MaxValue / 2) + 1
    val numExpectedSamples = 2
    def createIterator = Iterator.range(0, Int.MaxValue, step)
    assertEquals(numExpectedSamples, createIterator.count(_ => true))
    assertEquals(0, createIterator.min)
    assertEquals(step, createIterator.max)
  }
  @Test def rangeOverflow3() : Unit = {
    val step = 1000000000
    val numExpectedSamples = 5
    def createIterator = Iterator.range(Int.MinValue +10,Int.MaxValue - 10, step)
    assertEquals(numExpectedSamples, createIterator.count(_ => true))
    assertEquals(Int.MinValue + 10, createIterator.min)
    assertEquals(Int.MinValue + 10 + (numExpectedSamples - 1) * step, createIterator.max)
  }
  @Test def rangeUnderflow() : Unit = {
    val step = -100000000
    val numExpectedSamples = 22
    def createIterator = Iterator.range(0, -Int.MaxValue, step)
    assertEquals(numExpectedSamples, createIterator.count(_ => true))
    assertEquals((numExpectedSamples - 1) * step, createIterator.min)
    assertEquals(0, createIterator.max)
  }
  @Test def rangeUnderflow2() : Unit = {
    val step = -(Int.MaxValue / 2) - 1
    val numExpectedSamples = 2
    def createIterator = Iterator.range(0, -Int.MaxValue, step)
    assertEquals(numExpectedSamples, createIterator.count(_ => true))
    assertEquals(step, createIterator.min)
    assertEquals(0, createIterator.max)
  }
  @Test def rangeUnderflow3() : Unit = {
    val step = -1000000000
    val numExpectedSamples = 5
    def createIterator = Iterator.range(Int.MaxValue -10,Int.MinValue + 10,step)
    assertEquals(numExpectedSamples, createIterator.count(_ => true))
    assertEquals(Int.MaxValue - 10 + (numExpectedSamples - 1) * step, createIterator.min)
    assertEquals(Int.MaxValue - 10, createIterator.max)
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

  @Test def lazyListIsLazy(): Unit = {
    val results = mutable.ListBuffer.empty[Int]
    def mkIterator = Range.inclusive(1, 5).iterator map (x => { results += x ; x })
    def mkInfinite = Iterator continually { results += 1 ; 1 }

    val s1 = LazyList.from(mkIterator)
    val s2 = LazyList.from(mkInfinite)
    // back and forth without slipping into nontermination.
    results += LazyList.from(1).iterator.drop(10).to(LazyList).drop(10).iterator.next()
    assertTrue(List(21).sameElements(results))
  }

  // scala/bug#3516
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
  // scala/bug#8552
  @Test def indexOfShouldWorkForTwoParams(): Unit = {
    assertEquals(1, List(1, 2, 3).iterator.indexOf(2, 0))
    assertEquals(-1, List(5 -> 0).iterator.indexOf(5, 0))
    assertEquals(0, List(5 -> 0).iterator.indexOf((5, 0)))
    assertEquals(-1, List(5 -> 0, 9 -> 2, 0 -> 3).iterator.indexOf(9, 2))
    assertEquals(1, List(5 -> 0, 9 -> 2, 0 -> 3).iterator.indexOf(9 -> 2))
  }
  // scala/bug#9332
  @Test def spanExhaustsLeadingIterator(): Unit = {
    def it = Iterator.iterate(0)(_ + 1).take(6)
    val (x, y) = it.span(_ != 1)
    val z = x.toList
    assertEquals(1, z.size)
    assertFalse(x.hasNext)
    assertEquals(1, y.next())
    assertFalse(x.hasNext)   // was true, after advancing underlying iterator
  }
  // scala/bug#9913
  @Test def `span leading iterator finishes at state -1`(): Unit = {
    val (yes, no) = Iterator(1, 2, 3).span(_ => true)
    assertFalse(no.hasNext)
    assertTrue(yes.hasNext)
  }
  // scala/bug#9623
  @Test def noExcessiveHasNextInJoinIterator: Unit = {
    var counter = 0
    val exp = List(1,2,3,1,2,3)
    def it: Iterator[Int] = new Iterator[Int] {
      val parent = List(1,2,3).iterator
      def next(): Int = parent.next()
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
  // scala/bug#9691
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
  // scala/bug#11153
  @Test def handleExhaustedConcatSubIterator(): Unit = {
    val it = Iterator.empty ++ Iterator.empty
    // exhaust and clear internal state
    it.hasNext
    val concat = Iterator.empty ++ it
    while (concat.hasNext) concat.next()
  }

  @Test
  def hasCorrectDistinct: Unit = {
    val result = List(1, 1, 2, 3, 3, 3, 4, 5, 5).iterator.distinct

    assertTrue(result.hasNext)
    assertEquals(1, result.next())
    assertTrue(result.hasNext)
    assertEquals(2, result.next())
    assertTrue(result.hasNext)
    assertEquals(3, result.next())
    assertTrue(result.hasNext)
    assertEquals(4, result.next())
    assertTrue(result.hasNext)
    assertEquals(5, result.next())
    assertFalse(result.hasNext)
  }

  @Test
  def hasCorrectDistinctBy: Unit = {
    val result = List("a", "aa", "aaa", "b", "bb", "bbb", "bbbb", "c").iterator.distinctBy(_.length)

    assertTrue(result.hasNext)
    assertEquals("a", result.next())
    assertTrue(result.hasNext)
    assertEquals("aa", result.next())
    assertTrue(result.hasNext)
    assertEquals("aaa", result.next())
    assertTrue(result.hasNext)
    assertEquals("bbbb", result.next())
    assertFalse(result.hasNext)
  }

  @Test
  def knownSize: Unit = {

    def indexedSeq[A](xs: IndexedSeq[A]): Unit = {
      val it = xs.iterator
      assertEquals(xs.size, it.knownSize)
      it.next()
      assertEquals(xs.size - 1, it.knownSize)
    }

    indexedSeq(Vector(1, 2, 3))
    indexedSeq(mutable.ArrayBuffer(1, 2, 3))
    indexedSeq(immutable.ArraySeq(1, 2, 3))
    indexedSeq(Range(start = 1, end = 3, step = 1))
    indexedSeq(Range(start = 9, end = 2, step = -2))
    indexedSeq(immutable.NumericRange(start = 1, end = 3, step = 1))
    indexedSeq(immutable.NumericRange(start = -10, end = -5, step = 1))
  }

  private def knownSizeDecreases[A](it: Iterator[A]): Unit = {
    val size = it.knownSize
    it.next()
    assertEquals(size - 1, it.knownSize)
  }

  @Test
  def knownSize2: Unit = {
    assertEquals(10, Iterator.fill(10)(1).knownSize)
    assertEquals(0, Iterator.fill(-10)(1).knownSize)
    knownSizeDecreases(Iterator.fill(10)(1))

    assertEquals(10, Iterator.tabulate(10)(_.toString).knownSize)
    assertEquals(0, Iterator.tabulate(-10)(_.toString).knownSize)
    knownSizeDecreases(Iterator.tabulate(10)(_.toString))

    assertEquals(10, Iterator.range(1, 11).knownSize)
    knownSizeDecreases(Iterator.range(1, 11))
    assertEquals(5, Iterator.range(1, 11, 2).knownSize)
    assertEquals(4, Iterator.range(1, 11, 3).knownSize)
    assertEquals(5, Iterator.range(1, 10, 2).knownSize)
    assertEquals(3, Iterator.range(1, 10, 3).knownSize)
    knownSizeDecreases(Iterator.range(1, 10, 3))
    assertEquals(4, Iterator.range(-5, 5, 3).knownSize)
    assertEquals(4, Iterator.range(-15, -5, 3).knownSize)
    assertEquals(-1, Iterator.range(Int.MinValue, Int.MaxValue).knownSize)
    assertEquals(-1, Iterator.range(Int.MinValue, Int.MaxValue, 2).knownSize)
    assertEquals(1431655765, Iterator.range(Int.MinValue, Int.MaxValue, 3).knownSize)
    assertEquals(Int.MaxValue, Iterator.range(Int.MinValue, Int.MaxValue - 1, 2).knownSize)
  }

  @Test
  def knownSize3: Unit = {
    def it = Iterator.fill(10)(1)

    val buf = it.buffered
    assertEquals(10, buf.knownSize)
    buf.head
    assertEquals(10, buf.knownSize)
    knownSizeDecreases(buf)
    val buf2 = Iterator.continually(1).buffered
    assertEquals(-1, buf2.knownSize)
    buf2.head
    assertEquals(-1, buf2.knownSize)

    assertEquals(10, it.padTo(5, 0).knownSize)
    assertEquals(15, it.padTo(15, 0).knownSize)
    knownSizeDecreases(it.padTo(15, 0))

    val sl = it.scanLeft(0)(_ + _)
    assertEquals(11, sl.knownSize)
    knownSizeDecreases(sl)
    knownSizeDecreases(sl) // first element is special so check twice

    assertEquals(10, it.map(_ + 1).knownSize)
    knownSizeDecreases(it.map(_ + 1))

    assertEquals(5, it.zip(it.take(5)).knownSize)
    assertEquals(5, it.take(5).zip(it).knownSize)
    knownSizeDecreases(it.zip(it.take(5)))

    assertEquals(10, it.zipAll(it.take(5), 2, 3).knownSize)
    assertEquals(10, it.take(5).zipAll(it, 2, 3).knownSize)
    knownSizeDecreases(it.zipAll(it.take(5), 2, 3))

    val (a, b) = it.duplicate
    assertEquals(10, a.knownSize)
    assertEquals(10, b.knownSize)
    knownSizeDecreases(a)
    assertEquals(10, b.knownSize)
    knownSizeDecreases(b)
    knownSizeDecreases(b)
    assertEquals(9, a.knownSize)

    assertEquals(10, it.zipWithIndex.knownSize)
    knownSizeDecreases(it.zipWithIndex)
  }

  @Test
  def sliceKnownSize: Unit = {
    def it = Iterator.fill(10)(1)

    assertEquals(4, it.take(4).knownSize)
    assertEquals(10, it.take(30).knownSize)

    assertEquals(6, it.drop(4).knownSize)
    assertEquals(0, it.drop(15).knownSize)

    assertEquals(2, it.slice(4, 6).knownSize)
    assertEquals(2, it.slice(8, 15).knownSize)
    assertEquals(5, it.slice(-5, 5).knownSize)

    assertEquals(-1, Iterator.continually(1).take(5).knownSize)
    assertEquals(-1, List.fill(10)(1).take(5).knownSize)

    assertEquals(3, new Iterator.SliceIterator(it, 7, -1).knownSize)

    knownSizeDecreases(it.slice(2, 9))
    knownSizeDecreases(new Iterator.SliceIterator(it, 7, -1))
  }

  @Test
  def emptyKnownSize(): Unit = {
    assertEquals(0, Iterator.empty.knownSize)
  }

  @Test
  def mkString: Unit = {
    val it = List("a", null, "b", null, "c", null).iterator

    assertEquals("a,null,b,null,c,null", it.mkString(","))
  }

  @Test
  def emptyTypedIteratorsShouldBeEqual: Unit = {
    val emptyDoubleIterator = Iterator.empty[Double]
    val emptyIntIterator = Iterator.empty[Int]
    assertSame(emptyDoubleIterator, emptyIntIterator)
  }

  @Test
  def emptyIteratorInHigherOrderFunctions: Unit = {
    val seqOfIterators = Seq(Seq(1, 2, 3).iterator, Seq(3, 2, 1).iterator, Seq(1, 3, 2).iterator)
    val unified = seqOfIterators.foldLeft(Iterator.empty[Int])((a, b) => a ++ b)
    assertEquals(List(1, 2, 3, 3, 2, 1, 1, 3, 2), List.from(unified))
  }

  @Test
  def emptyIteratorBuilder: Unit = {
    assertSame(Iterator.empty[Int], Iterator.newBuilder[Int].result())
  }

  @Test
  def nonEmptyIteratorBuilder: Unit = {
    var iteratorBuilder = Iterator.newBuilder[Int]
    iteratorBuilder += 5
    iteratorBuilder += 4
    iteratorBuilder += 3
    assertEquals(List(5, 4, 3), List.from(iteratorBuilder.result()))
  }

  @Test
  def nonEmptyIteratorAndClearBuilder: Unit = {
    var iteratorBuilder = Iterator.newBuilder[Int]
    iteratorBuilder += 1
    iteratorBuilder.clear()
    assertSame(Iterator.empty, iteratorBuilder.result())
  }

  @Test def partition: Unit = {
    val it = Iterator(1, 2, 3, 4, 5, 6, 7)
    val (even, odd) = it.partition(n => (n & 1) == 0)
    assertSameElements(List.from(even), List(2, 4, 6))
    assertSameElements(List.from(odd), List(1, 3, 5, 7))
  }

  @Test def padTo: Unit = {
    val it = Iterator(2, 4, 6, 8)
    val padded = it.padTo(7, 10)
    assertSameElements(List.from(padded), List(2, 4, 6, 8, 10, 10, 10))
  }

  @Test def corresponds: Unit = {
    val it = Iterator(1, 2, 3, 4, 5)
    assertTrue(it.corresponds(Seq(1, 4, 9, 16, 25)) { (a, b) => b == a*a })
  }

  @Test def aggregate: Unit = {
    val result = Iterator('a', 'b', 'c').aggregate(0)({ (sum, ch) => sum + ch.toInt }, { (p1, p2) => p1 + p2 })
    assertEquals(result, 294)
  }

  @Test def copyToArray(): Unit = {
    def check(a: Array[Int], copyTo: Array[Int] => Int, elemsWritten: Int, start: Int, end: Int): Unit = {

      val copied = copyTo(a)
      assertEquals(elemsWritten, copied)

      var i = 0
      while (i < start) {
        assertEquals(a(i), 0)
        i += 1
      }
      while (i < a.length && i < end) {
        assertEquals(a(i), i - start)
        i += 1
      }
      while (i < a.length) {
        assertEquals(a(i), 0)
        i += 1
      }
    }

    val far = 100000
    def l = Iterable.from(Range(0, 100)).iterator
    check(new Array(100), l.copyToArray(_), 100, 0, far)
    check(new Array(10), l.copyToArray(_), 10, 0, far)
    check(new Array(1000), l.copyToArray(_), 100, 0, 100)

    check(new Array(100), l.copyToArray(_, 5), 95, 5, 105)
    check(new Array(10), l.copyToArray(_, 5), 5, 5, 10)
    check(new Array(1000), l.copyToArray(_, 5), 100, 5, 105)

    check(new Array(100), l.copyToArray(_, 5, 50), 50, 5, 55)
    check(new Array(10), l.copyToArray(_, 5, 50), 5, 5, 10)
    check(new Array(1000), l.copyToArray(_, 5, 50), 50, 5, 55)

    assertThrows[ArrayIndexOutOfBoundsException](l.copyToArray(new Array(10), -1))
    assertThrows[ArrayIndexOutOfBoundsException](l.copyToArray(new Array(10), -1, 10))

    check(new Array(10), l.copyToArray(_, 10), 0, 0, 0)
    check(new Array(10), l.copyToArray(_, 10, 10), 0, 0, 0)
    check(new Array(10), l.copyToArray(_, 0, -1), 0, 0, 0)
  }

  // scala/bug#10709
  @Test def `scan is lazy enough`(): Unit = {
    val results = collection.mutable.ListBuffer.empty[Int]
    val it = new AbstractIterator[Int] {
      var cur = 1
      val max = 3
      override def hasNext = {
        results += -cur
        cur < max
      }
      override def next() = {
        val res = cur
        results += -res
        cur += 1
        res
      }
    }
    val xy = it.scanLeft(10)((sum, x) => {
      results += -(sum + x)
      sum + x
    })
    val scan = collection.mutable.ListBuffer.empty[Int]
    for (i <- xy) {
      scan += i
      results += i
    }
    assertSameElements(List(10,11,13), scan)
    assertSameElements(List(10,-1,-1,-11,11,-2,-2,-13,13,-3), results)
  }

  @Test def unfoldCorrectness(): Unit = {
    val it1 = Iterator.unfold(1)(i => if (i > 10) None else Some((i, i + 1)))
    assertSameElements(1 to 10, it1)

    val it2 = Iterator.unfold(0)(_ => None)
    assertSameElements(Nil, it2)
  }

  @Test def unfoldLaziness(): Unit = {
    var executed: Boolean = false
    val it = Iterator.unfold(0)(_ => {executed = true; None})
    assertFalse(executed)
    it.toList
    assertTrue(executed)
  }

  @Test def `flatMap is memory efficient in previous element`(): Unit = {
    import java.lang.ref._
    import scala.util.chaining._
    // Array.iterator holds onto array reference; by contrast, iterating over List walks tail.
    // Avoid reaching seq1 through test class. Avoid testing Array.iterator.
    class C extends Iterable[String] {
      val ss = Array("first", "second")

      def iterator = new Iterator[String] {
        var i = 0

        def hasNext = i < ss.length

        def next() = if (hasNext) ss(i).tap(_ => i += 1) else Iterator.empty.next()
      }

      def apply(i: Int) = ss(i)
    }
    val seq1 = new WeakReference(new C)
    val seq2 = List("third")
    val it0: Iterator[Int] = Iterator(1, 2)
    lazy val it: Iterator[String] = it0.flatMap {
      case 1 => Option(seq1.get).getOrElse(Nil)
      case 2 => check(); seq2
      case _ => ???
    }

    def check() = assertNotReachable(seq1.get, it)(())

    def checkHasElement() = assertNotReachable(Option(seq1.get).map(_.apply(1)).orNull, it)(())

    assert(it.hasNext)
    assertEquals("first", it.next())

    // verify that we're in the middle of seq1
    assertThrows[AssertionError](checkHasElement())
    assertThrows[AssertionError](check())
    assert(it.hasNext)
    assertEquals("second", it.next())

    assert(it.hasNext)
    assertNotReachable(seq1.get, it) {
      assertEquals("third", it.next())
    }
    assert(!it.hasNext)
  }

  @Test def tapEach(): Unit = {
    locally {
      var i = 0
      val tapped = Iterator(-1, -1, -1).tapEach(_ => i += 1)
      assertEquals(true, tapped.hasNext)
      assertEquals(0, i)
    }

    locally {
      var i = 0
      val tapped = Iterator(-1, -1, -1).tapEach(_ => i += 1)
      assertEquals(-3, tapped.sum)
      assertEquals(3, i)
    }

    locally {
      var i = 0
      val tapped = Iterator(-1, -1, -1).tapEach(_ => i += 1)
      assertEquals(-1, tapped.next())
      assertEquals(1, i)
    }
  }

  @Test
  def t11106(): Unit = {
    var i = 0
    Iterator.continually(0)
      .map(_ => {i += 1; i})
      .withFilter(_ < 10)
      .take(3)
      .foreach(_ => ())
    assertEquals(3, i)
  }

  @Test
  def flatMap(): Unit = {
    def check[T](mkIterator: () => Iterator[T], expected: Array[T]): Unit = {
      // tests that the iterator produces the expected array of elems, when alternating hasNext/next() calls
      // then continues to be empty after repeated calls to hasNext/next after exhausted
      // additional variants are included, where we:
      // * avoid calls to hasNext, in this case `next()` should still work as expected
      // * avoid calls to hasNext in the post-exhaustion check -- next() should still throw each time
      // * avoid calls to next() in the post-exhaustion check -- hasNext should still return `false` each time
      locally {
        val iter = mkIterator()
        var i = 0
        while (i < expected.length) {
          assert(iter.hasNext)
          assertEquals(expected(i), iter.next())
          i += 1
        }
        i = 0
        while (i < 10) {
          assertThrows[Exception](iter.next())
          assert(!iter.hasNext)
          i += 1
        }
      }
      locally {
        val iter = mkIterator()
        var i = 0
        while (i < expected.length) {
          assertEquals(expected(i), iter.next())
          i += 1
        }
        i = 0
        while (i < 10) {
          assertThrows[Exception](iter.next())
          assert(!iter.hasNext)
          i += 1
        }
      }
      locally {
        val iter = mkIterator()
        var i = 0
        while (i < expected.length) {
          assertEquals(expected(i), iter.next())
          i += 1
        }
        i = 0
        while (i < 10) {
          assertThrows[Exception](iter.next())
          i += 1
        }
      }
      locally {
        val iter = mkIterator()
        var i = 0
        while (i < expected.length) {
          assertEquals(expected(i), iter.next())
          i += 1
        }
        i = 0
        while (i < 10) {
          i += 1
          assert(!iter.hasNext)
        }
      }
    }

    check(() => Iterator.empty[Int].flatMap(_ => Iterator.empty[Int]), Array())
    check(() => Iterator.empty[Int].flatMap(_ => 1 to 10), Array())
    check(() => Iterator(1).flatMap(i => List(i + 1, i + 2)), Array(2, 3))
    check(() => Iterator(1).flatMap(i => List(i + 1, i + 2)), Array(2, 3))

    check(() => (0 to 100 by 10).iterator.flatMap(i => i to (i + 9)), (0 to 109).toArray)


    check(() => Iterator.from(1 to 10).flatMap {
      case 1 => Nil
      case 2 => List(1,2,3)
      case 3 => Nil
      case 4 => List(4)
      case 5 => List(5,6,7,8,9)
      case 6 => List(10)
      case 7 => List(11,12,13,14,15)
      case 8 => Nil
      case 9 => Nil
      case 10 => Nil
      case _ => Nil
    }, Array.from(1 to 15))
  }

  @Test
  def `t11807 multiply-merged concat iterators`(): Unit = {
    val it0 = Array(1).iterator
    val it1 = Array(2).iterator ++ Array(3).iterator
    val it2 = it0 ++ it1

    assertEquals(1, it2.next())
    assertTrue(it2.hasNext)

    val it3 = it2 ++ Array(4).iterator
    assertEquals(2, it3.next())
    assertEquals(3, it3.next())
    assertTrue("concatted tail of it3 should be next", it3.hasNext)
  }

}
