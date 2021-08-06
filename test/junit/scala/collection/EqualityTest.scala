package scala.collection

import org.junit.jupiter.api.{Assertions, Test}

import scala.collection.immutable.{LazyList, List, Range}
import scala.collection.mutable.ArrayBuffer

class EqualityTest {

  @Test
  def equality(): Unit = {
    val list: Iterable[Int] = List(1, 2, 3)
    val lazyList: Iterable[Int] = LazyList(1, 2, 3)
    val buffer = ArrayBuffer(1, 2, 3)
    val range = Range.inclusive(1, 3)
    Assertions.assertTrue(list == lazyList)
    Assertions.assertTrue(list.## == lazyList.##)
    Assertions.assertTrue(list == (buffer: Iterable[Int]))
    Assertions.assertTrue(list.## == buffer.##)
    Assertions.assertTrue(list == (range: Iterable[Int]))
    Assertions.assertTrue(list.## == range.##)
    buffer += 4
    Assertions.assertTrue(list != (buffer: Iterable[Int]))
    Assertions.assertTrue(list.## != buffer.##)
  }

  @Test
  def viewsComparisonDoesntConsumeElements(): Unit = {
    val xs = List(1, 2, 3)
    var n = 0
    val xsView = xs.view.map { x => n += 1; x }
    Assertions.assertTrue(xs != xsView)
    Assertions.assertTrue(xsView != xs)
    Assertions.assertTrue(xs.## != xsView.##)
    Assertions.assertEquals(0, n) // The view hasn’t been evaluated
    Assertions.assertTrue(xs sameElements xsView) // sameElements does evaluate the view elements
    Assertions.assertEquals(3, n)
  }

}
