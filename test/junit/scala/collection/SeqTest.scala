package scala.collection

import org.junit.Assert._
import org.junit.Test

import scala.tools.testkit.{AllocationTest, CompileTime}

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
    assertEquals(-1, List(0, 1).indexOfSlice(List(1, 2)))
  }

  @Test
  def hasCorrectLastIndexOfSlice(): Unit = {
    assertEquals(0, Vector(0, 1).lastIndexOfSlice(List(0, 1)))
    assertEquals(0, Vector(0, 1).lastIndexOfSlice(Vector(0, 1)))
    assertEquals(4, Vector(0, 1, 2, 0, 1, 2).lastIndexOfSlice(Vector(1, 2)))
    assertEquals(1, Vector(0, 1, 2, 0, 1, 2).lastIndexOfSlice(Vector(1, 2), end = 3))
    assertEquals(-1, List(0, 1).lastIndexOfSlice(List(1, 2)))
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
}
