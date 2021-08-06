package scala.collection

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class IndexedSeqViewTest {
  @Test
  def _toString(): Unit = {
    assertEquals("IndexedSeqView(<not computed>)", IndexedSeq(1, 2, 3).view.toString)
  }

  @Test
  def iteratorKnownSize(): Unit = {
    assertEquals(5, IndexedSeq(1, 2, 3, 4, 5).view.iterator.knownSize)
    assertEquals(2, IndexedSeq(1, 2, 3, 4, 5).view.iterator.take(2).knownSize)
    assertEquals(2, IndexedSeq(1, 2, 3, 4, 5).view.iterator.slice(2, 4).knownSize)
  }
}
