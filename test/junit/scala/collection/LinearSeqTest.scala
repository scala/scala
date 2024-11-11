package scala.collection

import org.junit.Test
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import scala.tools.testkit.AssertUtil.assertThrows

class LinearSeqTest {
  import LinearSeqTest._

  // Tests regression on issue 11262
  @Test def `iterator of trivial subclass`: Unit = {
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
  @Test def `length works`: Unit = assertEquals(3, ConstantLinearSeq(3, "").length)
  @Test def `foreach works`: Unit = {
    var res = 0
    ConstantLinearSeq(0, "42").foreach(res += _.toInt)
    assertEquals(0, res)
    ConstantLinearSeq(3, "42").foreach(res += _.toInt)
    assertEquals(126, res)
  }
  @Test def `forall works`: Unit = {
    assertTrue(ConstantLinearSeq(3, "").forall(_.isEmpty))
    assertFalse(ConstantLinearSeq(3, "x").forall(_.isEmpty))
  }
  @Test def `exists works`: Unit = {
    var count = 0
    assertTrue(ConstantLinearSeq(3, "").exists { x => count += 1; x.isEmpty })
    assertEquals(1, count)
    count = 0
    assertFalse(ConstantLinearSeq(3, "x").exists { x => count += 1; x.isEmpty })
    assertEquals(3, count)
  }
  @Test def `contains works`: Unit = {
    assertTrue(ConstantLinearSeq(3, "x", limit = 0).contains("x"))
    assertFalse(ConstantLinearSeq(3, "x", limit = 3).contains("y"))
  }
  @Test def `find works`: Unit = {
    assertEquals(Some("x"), ConstantLinearSeq(3, "x", limit = 0).find(_ == "x"))
    assertEquals(None, ConstantLinearSeq(3, "x", limit = 3).find(_ == "y"))
  }
  @Test def `foldLeft works`: Unit =
    assertEquals(126, ConstantLinearSeq(3, 42).foldLeft(0)(_ + _))
  @Test def `sameElements works`: Unit =
    assertTrue(ConstantLinearSeq(3, 42, limit = 3).sameElements(List(42, 42, 42)))
  @Test def `segmentLength works`: Unit =
    assertEquals(3, ConstantLinearSeq(5, 42).segmentLength(_ == 42, from = 2))
  @Test def `indexWhere works`: Unit =
    assertEquals(3, ArrayLinearSeq(Array(1, 2, 3, 4, 5)).indexWhere(_ > 3))
  @Test def `lastIndexWhere works`: Unit =
    assertEquals(4, ArrayLinearSeq(Array(1, 2, 3, 3, 3, 4, 5)).lastIndexWhere(_ == 3))
  @Test def `findLast works`: Unit =
    assertEquals(Some(3), ArrayLinearSeq(Array(1, 2, 3, 4, 3, 5, 3, 4, 5)).findLast(_ < 4))
}
object LinearSeqTest {
  private case class ConstantLinearSeq[A](len: Int, elem: A, limit: Int = Int.MaxValue) extends LinearSeq[A] {
    require(len >= 0)
    override val isEmpty: Boolean = len == 0
    override val head = elem
    override lazy val tail =
      if (limit == 0) throw new NoSuchElementException("ConstantLinearSeq.limit")
      else if (isEmpty) throw new NoSuchElementException("ConstantLinearSeq.tail")
      else copy(len = len - 1, limit = limit - 1)
  }
  private case class ArrayLinearSeq[A](values: Array[A], index: Int = 0, limit: Int = Int.MaxValue) extends LinearSeq[A] {
    override val isEmpty: Boolean = index == values.length
    override lazy val head = values(index)
    override lazy val tail =
      if (limit == 0) throw new NoSuchElementException("ArrayLinearSeq.limit")
      else if (isEmpty) throw new NoSuchElementException("ArrayLinearSeq.tail")
      else copy(index = index + 1, limit = limit - 1)
  }
}
