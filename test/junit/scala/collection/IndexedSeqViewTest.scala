package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.AssertUtil._

@RunWith(classOf[JUnit4])
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

  @Test
  def reverseEmptyIterator(): Unit = {
    assertEquals(0, Vector.empty[Int].reverseIterator.take(1).toList.size)
  }

  @Test def `t9069 slice is stack safe`: Unit = {
    import scala.annotation.tailrec
    import scala.collection.mutable

    val coll = Array.fill(200)(0)

    type View = collection.View[Int]
    //type View = mutable.IndexedSeqView[Int, mutable.IndexedSeq[Int]]  // 2.12, as reported

    def makeStackedSlice(coll: mutable.IndexedSeq[Int], count: Int) = {
      @tailrec def makeSlicesOfView(view: View, noOfSlices: Int): View =
        if (noOfSlices == 0) probeStackSafety[View]() else makeSlicesOfView(view.tail, noOfSlices - 1)

      makeSlicesOfView(coll.view, count)
    }

    assertStackSafe(makeStackedSlice(coll, 50), makeStackedSlice(coll, 100))
  }
}
