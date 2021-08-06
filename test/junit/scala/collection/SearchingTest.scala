package scala.collection

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import scala.collection.Searching._

class SearchingTest {

  @Test
  def doesLinearSearchOnLinearSeqs(): Unit = {

    class TestSeq[A](list: List[A]) extends Seq[A] {
      var elementsAccessed = immutable.Set.empty[Int]

      protected[this] def newBuilder = ??? // not needed for this test
      def iterator = list.iterator
      def length = list.length
      def apply(idx: Int) = { elementsAccessed += idx; list(idx) }
    }

    val coll = new TestSeq((0 to 6).toList)

    assertEquals(Found(5), coll.search(5))
    assertEquals(Set.empty, coll.elementsAccessed) // linear search should not access elements via apply()
  }

  @Test
  def doesBinarySearchOnIndexedSeqs(): Unit = {

    class TestIndexedSeq[A](vec: Vector[A]) extends IndexedSeq[A] {
      var elementsAccessed = immutable.Set.empty[Int]

      protected[this] def newBuilder = ??? // not needed for this test
      def length = vec.length
      def apply(idx: Int) = { elementsAccessed += idx; vec(idx) }
    }

    val coll = new TestIndexedSeq((0 to 6).toVector)

    assertEquals(Found(5), coll.search(5))
    assertEquals(Set(3, 5), coll.elementsAccessed)
  }

  @Test def searchWorks(): Unit = {
    val ls = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 13)
    assertEquals(Found(2), ls.search(3))
    assertEquals(Found(4), ls.search(5, 3, 8))
    assertEquals(InsertionPoint(10), ls.search(12))

    val is = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 13)
    assertEquals(Found(2), is.search(3))
    assertEquals(Found(4), is.search(5, 3, 8))
    assertEquals(InsertionPoint(10), is.search(12))
  }
}
