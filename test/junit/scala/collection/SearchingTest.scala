package scala.collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert._
import org.junit.Test

import scala.collection.Searching._

@RunWith(classOf[JUnit4])
class SearchingTest {
  class TestSeq[A](list: List[A]) extends SeqLike[A, TestSeq[A]] {
    var elementsAccessed = Set.empty[Int]

    protected[this] def newBuilder = ??? // not needed for this test
    def seq = list
    def iterator = list.iterator
    def length = list.length
    def apply(idx: Int) = { elementsAccessed += idx; list(idx) }
  }

  @Test
  def doesLinearSearchOnLinearSeqs() {
    val coll = new TestSeq((0 to 6).toList)

    assertEquals(Found(5), coll.search(5))
    assertEquals(Set.empty, coll.elementsAccessed) // linear search should not access elements via apply()
  }

  class TestIndexedSeq[A](vec: Vector[A]) extends IndexedSeqLike[A, TestIndexedSeq[A]] {
    var elementsAccessed = Set.empty[Int]

    protected[this] def newBuilder = ??? // not needed for this test
    def seq = vec
    def length = vec.length
    def apply(idx: Int) = { elementsAccessed += idx; vec(idx) }
  }

  @Test
  def doesBinarySearchOnIndexedSeqs() {
    val coll = new TestIndexedSeq((0 to 6).toVector)
    assertEquals(Found(5), coll.search(5))
    assertEquals(Set(3, 5), coll.elementsAccessed)
  }

  @Test
  def doesLinearSearchOnLinerSeqWithLimitedRange(): Unit = {
    val coll = (0 to 6).toList
    assertEquals(Found(5), coll.search(5, 3, 6))
    assertEquals(InsertionPoint(5), coll.search(5, 0, 5))
  }

  @Test
  def doesLinearSearchOnIndexedSeqsWithLimitedRange(): Unit = {
    val coll = (0 to 6).toVector
    assertEquals(Found(5), coll.search(5, 5, 6))
    assertEquals(InsertionPoint(5), coll.search(5, 0, 5))
  }

  @Test
  def doesSearchByOnLinearSeq(): Unit = {
    val coll = (0 to 5).toList
    assertEquals(Found(1), coll.searchBy("1", _.toString))
    assertEquals(InsertionPoint(0), coll.searchBy("-1", _.toString))
  }

  @Test
  def doesSearchByOnIndexedSeq(): Unit = {
    val coll = (0 to 5).toVector
    assertEquals(Found(1), coll.searchBy("1", _.toString))
    assertEquals(InsertionPoint(6), coll.searchBy("6", _.toString))
  }

  @Test
  def doesSearchByOnLinerSeqWithLimitedRange():Unit = {
    val coll = (0 to 5).toList
    assertEquals(Found(3), coll.searchBy("3", 2, 5, _.toString))
    assertEquals(InsertionPoint(3), coll.searchBy("6", 2, 3, _.toString))
  }
}
