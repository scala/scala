package scala.collection

import org.junit.Assert.{assertThrows => _, _}
import org.junit.Test

import scala.collection.immutable.LazyList
import scala.tools.testkit.{AllocationTest, AssertUtil, CompileTime}, AssertUtil.assertThrows

class SeqTest extends AllocationTest {

  @Test def `t9936 indexWhere`(): Unit = {
    assertEquals(2, "abcde".indexOf('c', -1))
    assertEquals(2, "abcde".indexOf('c', -2))
    assertEquals(2, "abcde".toVector.indexOf('c', -1))
    assertEquals(2, "abcde".toVector.indexOf('c', -2))
    assertEquals(2, "abcde".toVector.indexWhere(_ == 'c', -1))
    assertEquals(2, "abcde".toVector.indexWhere(_ == 'c', -2))
  }

  @Test def combinations(): Unit = {
    assertEquals(List(Nil), Nil.combinations(0).toList)
    assertEquals(Nil, Nil.combinations(1).toList)
    assertEquals(List(List(1, 2), List(1, 3), List(2, 3)), List(1, 2, 3).combinations(2).toList)
    assertEquals(List(List(1, 2, 3)), List(1, 2, 3).combinations(3).toList)
  }

  @Test
  def hasCorrectDistinct(): Unit = {
    assertEquals(Seq(1, 2, 3, 4, 5), Seq(1, 1, 2, 3, 3, 3, 4, 5, 5).distinct)
  }

  @Test
  def hasCorrectDistinctBy(): Unit = {
    val result = Seq("a", "aa", "aaa", "b", "bb", "bbb", "bbbb", "c").distinctBy(_.length)

    assertEquals(Seq("a", "aa", "aaa", "bbbb"), result)
  }

  @Test
  def hasCorrectIndexOfSlice(): Unit = {
    assertEquals(0, Vector(0, 1).indexOfSlice(List(0, 1)))
    assertEquals(0, Vector(0, 1).indexOfSlice(Vector(0, 1)))
    assertEquals(1, Vector(0, 1, 2, 0, 1, 2).indexOfSlice(Vector(1, 2)))
    assertEquals(4, Vector(0, 1, 2, 0, 1, 2).indexOfSlice(Vector(1, 2), from = 2))
    assertEquals(1, Vector(0, 1, 2).indexOfSlice(Vector(1, 2), from = -1))
    assertEquals(-1, List(0, 1).indexOfSlice(List(1, 2)))
    assertEquals(1, List(0).indexOfSlice(Nil, from = 1))
    assertEquals(-1, List(0).indexOfSlice(Nil, from = 2))
    assertEquals(0, List(0).indexOfSlice(Nil, from = -1))
    assertEquals(1, List(0, 1, 2).indexOfSlice(List(1, 2), from = -1))
  }

  // Exercises KMP backtracking: the pattern has a self-overlapping prefix (1,2,1,2,3),
  // which forces the matcher to reuse the jump table rather than restart from scratch.
  @Test
  def kmpBacktrackingIndexOfSlice(): Unit = {
    // pattern (1,2,1,2,3) inside a source that contains several false partial matches
    val source = List(1, 2, 1, 2, 1, 2, 3, 4)
    val pattern = List(1, 2, 1, 2, 3)
    assertEquals(2, source.indexOfSlice(pattern))                            // non-indexed source
    assertEquals(2, source.toVector.indexOfSlice(pattern))                   // indexed source
    assertEquals(2, source.to(LazyList).indexOfSlice(pattern))               // lazy source
    assertEquals(2, source.indexOfSlice(pattern.to(LazyList)))               // lazy pattern
    assertEquals(2, source.to(LazyList).indexOfSlice(pattern.to(LazyList)))  // both lazy

    // no match: pattern has a trailing element that never appears
    val miss = List(1, 2, 1, 2, 3, 9)
    assertEquals(-1, source.indexOfSlice(miss))
    assertEquals(-1, source.to(LazyList).indexOfSlice(miss))
    assertEquals(-1, source.to(LazyList).indexOfSlice(miss.to(LazyList)))
  }

  @Test
  def kmpBacktrackingLastIndexOfSlice(): Unit = {
    // the only match of (1,2,3) starts at index 4
    val source = List(1, 2, 1, 2, 1, 2, 3, 1, 2)
    val pattern = List(1, 2, 3)
    assertEquals(4, source.lastIndexOfSlice(pattern))
    assertEquals(4, source.toVector.lastIndexOfSlice(pattern))
    assertEquals(4, source.to(LazyList).lastIndexOfSlice(pattern))
    // with end = 3 the single match at 4 is excluded
    assertEquals(-1, source.to(LazyList).lastIndexOfSlice(pattern, end = 3))
    assertEquals(-1, source.to(LazyList).lastIndexOfSlice(List(9, 9)))

    // two matches of (1,2,1,2) — latest starts at index 2 (exercises KMP jump table on backward pass)
    val src2 = List(1, 2, 1, 2, 1, 2, 3)
    val pat2 = List(1, 2, 1, 2)
    assertEquals(2, src2.lastIndexOfSlice(pat2))
    assertEquals(2, src2.toVector.lastIndexOfSlice(pat2))
    assertEquals(2, src2.to(LazyList).lastIndexOfSlice(pat2))
  }

  // LazyList pattern has knownSize == -1, so kmpSearch takes the "unknown pattern length"
  // branch that pre-scans the source for the pattern head before forcing W.length.
  @Test
  def indexOfSliceLazyPattern(): Unit = {
    val src = List(0, 1, 2, 3, 4, 5)
    assertEquals(2, src.indexOfSlice(LazyList(2, 3)))
    assertEquals(2, src.to(LazyList).indexOfSlice(LazyList(2, 3)))
    assertEquals(2, src.to(LazyList).indexOfSlice(LazyList(2, 3), from = 2))
    assertEquals(-1, src.to(LazyList).indexOfSlice(LazyList(2, 3), from = 3))
    assertEquals(-1, src.to(LazyList).indexOfSlice(LazyList(9, 9)))
    // single-element lazy pattern reaches the `clipped()` branch
    assertEquals(3, src.to(LazyList).indexOfSlice(LazyList(3)))
    assertEquals(-1, src.to(LazyList).indexOfSlice(LazyList(99)))
    // empty lazy pattern
    assertEquals(0, src.to(LazyList).indexOfSlice(LazyList.empty[Int]))
    assertEquals(2, src.to(LazyList).indexOfSlice(LazyList.empty[Int], from = 2))
    assertEquals(src.length, src.to(LazyList).indexOfSlice(LazyList.empty[Int], from = src.length))
    assertEquals(-1, src.to(LazyList).indexOfSlice(LazyList.empty[Int], from = src.length + 1))
  }

  @Test
  def hasCorrectLastIndexOfSlice(): Unit = {
    assertEquals(0, Vector(0, 1).lastIndexOfSlice(List(0, 1)))
    assertEquals(0, Vector(0, 1).lastIndexOfSlice(Vector(0, 1)))
    assertEquals(4, Vector(0, 1, 2, 0, 1, 2).lastIndexOfSlice(Vector(1, 2)))
    assertEquals(1, Vector(0, 1, 2, 0, 1, 2).lastIndexOfSlice(Vector(1, 2), end = 3))
    assertEquals(-1, List(0, 1).lastIndexOfSlice(List(1, 2)))
    // List (non-indexed) with explicit end
    assertEquals(4, List(0, 1, 2, 0, 1, 2).lastIndexOfSlice(List(1, 2)))
    assertEquals(1, List(0, 1, 2, 0, 1, 2).lastIndexOfSlice(List(1, 2), end = 3))
    assertEquals(-1, List(0, 1, 2, 0, 1, 2).lastIndexOfSlice(List(1, 2), end = -1))
    // LazyList source — (0,1,2,3,4,5,1,2) has matches at 1 and 6, latest is 6
    assertEquals(6, (0 to 5).to(LazyList).appendedAll(List(1, 2)).lastIndexOfSlice(List(1, 2)))
    assertEquals(1, LazyList(0, 1, 2, 0, 1, 2).lastIndexOfSlice(List(1, 2), end = 3))

    // empty pattern on non-indexed sources — must return the (clipped) length
    assertEquals(3, List(0, 1, 2).lastIndexOfSlice(Nil))
    assertEquals(3, LazyList(0, 1, 2).lastIndexOfSlice(Nil))
    assertEquals(2, List(0, 1, 2).lastIndexOfSlice(Nil, end = 2))
    assertEquals(-1, List(0, 1, 2).lastIndexOfSlice(Nil, end = -1))
  }

  // `knownSize` may be -1 even for an `IndexedSeq`, since -1 just means "not cheaply known".
  // `lastIndexOfSlice` must then not hand `kmpSearch` an `m1` past the end of the source.
  @Test
  def lastIndexOfSliceIndexedWithUnknownSize(): Unit = {
    class Unsized[A](u: Vector[A]) extends AbstractSeq[A] with IndexedSeq[A] {
      def apply(i: Int) = u(i)
      def length = u.length
      override def knownSize = -1
    }
    val empty = new Unsized(Vector.empty[Int])
    val two = new Unsized(Vector(1, 2))
    assertEquals(-1, empty.lastIndexOfSlice(List(0, 0), end = 1))
    assertEquals(-1, two.lastIndexOfSlice(List(9, 9), end = 5))
    assertEquals(0, two.lastIndexOfSlice(List(1, 2), end = 5))
    assertEquals(0, two.lastIndexOfSlice(List(1, 2)))
    assertEquals(1, two.lastIndexOfSlice(List(2), end = 5))   // single element, via `clipped`
    assertEquals(2, two.lastIndexOfSlice(Nil, end = 5))
    assertEquals(-1, empty.indexOfSlice(List(0, 0)))
    assertEquals(1, two.indexOfSlice(List(2), from = 1))
  }

  @Test
  def hasCorrectDiff(): Unit = {
    val s1 = Seq(1, 2, 3, 4, 5)
    val s2 = Seq(1, 3, 5, 7, 9)

    assertEquals(Seq(2, 4), s1.diff(s2))
  }

  @Test
  def hasCorrectIntersect(): Unit = {
    val s1 = Seq(1, 2, 3, 4, 5)
    val s2 = Seq(1, 3, 5, 7, 9)

    assertEquals(Seq(1, 3, 5), s1.intersect(s2))
  }

  @deprecated("Tests deprecated API", since="2.13")
  @Test
  def unionAlias(): Unit = {
    val s1 = Seq(1, 2, 3)
    val s2 = Seq(4, 5, 6)
    assertEquals(s1.concat(s2), s1.union(s2))
  }

  @Test
  def testLengthIs(): Unit = {
    val s = Seq(1, 2, 3)
    assert(s.lengthIs <= 3)
    assert(s.lengthIs == 3)
    assert(s.lengthIs >= 3)
    assert(s.lengthIs <= 4)
    assert(s.lengthIs < 4)
    assert(s.lengthIs != 4)
    assert(s.lengthIs >= 2)
    assert(s.lengthIs > 2)
    assert(s.lengthIs != 2)
  }

  @Test def emptyNonAllocating(): Unit = {
    nonAllocating(Seq.empty)
    nonAllocating(Seq())
  }

  @Test def smallSeqAllocation(): Unit = {
    if (CompileTime.versionNumberString == "2.13.2") return
    exactAllocates(Sizes.list * 1, "collection seq  size 1")(Seq("0"))
    exactAllocates(Sizes.list * 2, "collection seq  size 2")(Seq("0", "1"))
    exactAllocates(Sizes.list * 3, "collection seq  size 3")(Seq("0", "1", ""))
    exactAllocates(Sizes.list * 4, "collection seq  size 4")(Seq("0", "1", "2", "3"))
    exactAllocates(Sizes.list * 5, "collection seq  size 5")(Seq("0", "1", "2", "3", "4"))
    exactAllocates(Sizes.list * 6, "collection seq  size 6")(Seq("0", "1", "2", "3", "4", "5"))
    exactAllocates(Sizes.list * 7, "collection seq  size 7")(Seq("0", "1", "2", "3", "4", "5", "6"))
  }

  @Test def largeSeqAllocation(): Unit = {
    def expected(n: Int) = Sizes.list * n + Sizes.wrappedRefArray(n) + Sizes.wrappedRefArrayIterator
    exactAllocates(expected(10) , "collection seq size 10")(
      Seq("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
    exactAllocates(expected(20), "collection seq size 20")(
      Seq("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"))
  }

  /** A sequence of no consequence. */
  class Inconsequential[+A](n: Int) extends AbstractSeq[A] {
    def iterator: Iterator[A] = ???
    def apply(i: Int): A = ???
    def length: Int = knownSize
    override def knownSize = n
  }
  object Inconsequential {
    def apply(n: Int) = new Inconsequential(n)
  }
  type ??? = NotImplementedError

  @Test def `sameElements by size`: Unit = {
    assertFalse(Inconsequential(0).sameElements(Inconsequential(1)))
    assertFalse(Inconsequential(1).sameElements(Inconsequential(2)))
    assertTrue(Inconsequential(0).sameElements(Inconsequential(0)))
    assertThrows[???](Inconsequential(1).sameElements(Inconsequential(1)))
    assertThrows[???](Inconsequential(-1).sameElements(Inconsequential(-1)))
  }
}
