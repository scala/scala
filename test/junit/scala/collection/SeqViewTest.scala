package scala.collection

import org.junit.Assert.{assertThrows => _, _}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.AssertUtil.assertThrows

@RunWith(classOf[JUnit4])
class SeqViewTest {
  @Test
  def _toString(): Unit = {
    assertEquals("SeqView(<not computed>)", Seq(1, 2, 3).view.toString)
  }

  private def seq(n: Int, maxIterate: Int = -1): Seq[Int] =
    new AbstractSeq[Int] {
      val values = 1 to n
      var remaining = maxIterate
      def iterator = {
        if (remaining == 0) throw new IllegalStateException()
        if (remaining > 0) remaining -= 1
        values.iterator
      }
      def apply(i: Int) = values(i)
      def length = values.length
    }
  private def seqk(n: Int, maxIterate: Int): Seq[Int] =
    new AbstractSeq[Int] {
      val values = 1 to n
      var remaining = maxIterate
      def iterator = {
        if (remaining == 0) throw new IllegalStateException()
        if (remaining > 0) remaining -= 1
        values.iterator
      }
      def apply(i: Int) = values(i)
      def length = values.length
      override def knownSize = length
    }

  @Test def `sum of seq view`: Unit = {
    assertEquals(10, seq(4).view.sum)
  }
  @Test def `sum of seq view iterates once`: Unit = {
    val sut = seqk(4, 1).view
    assertEquals(10, sut.sum)
    assertThrows[IllegalStateException](sut.sum)
  }
}
