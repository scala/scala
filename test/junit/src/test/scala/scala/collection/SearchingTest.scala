package scala.collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert._
import org.junit.Test
import scala.collection.Searching._

@RunWith(classOf[JUnit4])
class SearchingTest {

  @Test
  def doesLinearSearchOnLinearSeqs() {

    class TestSeq[A](list: List[A]) extends SeqLike[A, TestSeq[A]] {
      var elementsAccessed = Set.empty[Int]

      protected[this] def newBuilder = ??? // not needed for this test
      def seq = list
      def iterator = list.iterator
      def length = list.length
      def apply(idx: Int) = { elementsAccessed += idx; list(idx) }
    }

    val coll = new TestSeq((0 to 6).toList)

    assertEquals(Found(5), coll.search(5))
    assertEquals(Set.empty, coll.elementsAccessed) // linear search should not access elements via apply()
  }

  @Test
  def doesBinarySearchOnIndexedSeqs() {

    class TestIndexedSeq[A](vec: Vector[A]) extends IndexedSeqLike[A, TestIndexedSeq[A]] {
      var elementsAccessed = Set.empty[Int]

      protected[this] def newBuilder = ??? // not needed for this test
      def seq = vec
      def length = vec.length
      def apply(idx: Int) = { elementsAccessed += idx; vec(idx) }
    }

    val coll = new TestIndexedSeq((0 to 6).toVector)

    assertEquals(Found(5), coll.search(5))
    assertEquals(Set(3, 5), coll.elementsAccessed)
  }
}
