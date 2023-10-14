package scala.collection

import org.junit.Test
import org.junit.Assert.{assertEquals, assertTrue}
import scala.tools.testkit.AssertUtil.assertThrows

class LinearSeqTest {
  // Tests regression on issue 11262
  @Test def extensionIteratorTest(): Unit = {
    class ConstantLinearSeq[A](len: Int, elt: A) extends scala.collection.LinearSeq[A] {
      override val isEmpty: Boolean = len == 0
      override val head = elt
      override lazy val tail = if(len > 0) new ConstantLinearSeq(len - 1, elt) else throw new Exception("tail of empty Seq")
    }

    val x = new ConstantLinearSeq(4, 7)

    val it = x.iterator // The main thing we want to test is that this does not throw an exception
    assertTrue(it.hasNext) // Call it at least once so that it won't be optimized away
  }

  // LinearSeqOps used isEmpty from SeqOps, which used lengthCompare, but LSO.lengthCompare used isEmpty
  @Test def `linear seq is incoherent`: Unit = {
    val ls = new LinearSeq[Int] {
      var count = 0
      override def isEmpty = {
        if (count > 5) throw new IllegalStateException("limit")
        count += 1
        super.isEmpty
      }
    }
    assertThrows[IllegalStateException](ls.toString, _ == "limit")
    assertThrows[IllegalStateException](ls.head, _ == "limit")
    assertThrows[IllegalStateException](ls.tail, _ == "limit")
  }
  @Test def `linear seq is semicoherent`: Unit = {
    val ls = new LinearSeq[Int] {
      override def isEmpty = true
    }
    assertEquals("LinearSeq()", ls.toString)
    assertThrows[NoSuchElementException](ls.head)
    assertThrows[UnsupportedOperationException](ls.tail)
  }
}
