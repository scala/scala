package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

object TraversableLikeTest {
  abstract class FakeIndexedSeq[A] extends IndexedSeq[A] {
    def apply(i: Int): A = ???
    def length: Int = 0
  }
}

@RunWith(classOf[JUnit4])
class TraversableLikeTest {
  import TraversableLikeTest._

  // For test_SI9019; out here because as of test writing, putting this in a method would crash compiler
  class Baz[@specialized(Int) A]() extends IndexedSeq[A] {
    def apply(i: Int) = ???
    def length: Int = 0
  }

  @Test
  def test_SI10631 {
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
