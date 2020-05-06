package scala.collection

import org.junit.Assert._
import org.junit.Test

class TraversableLikeTest {
  @Test
  def `SI10631 calling seqview.map.last evaluates head twice`(): Unit = {
    val baselist = List(1, 2)
    var checklist = List.empty[Int]
    val lst = baselist.view.map { x =>
      checklist = x :: checklist
      x
    }
    assertEquals(2, lst.last)
    assertEquals(baselist.reverse, checklist)
  }
}
