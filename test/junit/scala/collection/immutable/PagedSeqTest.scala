package scala.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

@RunWith(classOf[JUnit4])
class PagedSeqTest {
  // should not NPE, and should equal the given Seq
  @Test
  def test_SI6615(): Unit = {
    assertEquals(Seq('a'), PagedSeq.fromStrings(List.fill(5000)("a")).slice(4096, 4097))
  }

  // should not NPE, and should be empty
  @Test
  def test_SI9480(): Unit = {
    assertEquals(Seq(), PagedSeq.fromStrings(List("a")).slice(1))
  }

  // Slices shouldn't read outside where they belong
  @Test
  def test_SI6519 {
    var readAttempt = 0
    val sideEffectingIterator = new Iterator[Int] {
      def hasNext = readAttempt < 65536
      def next = { readAttempt += 1; readAttempt }
    }
    val s = PagedSeq.fromIterator(sideEffectingIterator).slice(0,2).mkString
    assertEquals(s, "12")
    assert(readAttempt <= 4096)
  }
}
