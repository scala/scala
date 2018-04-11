package scala.collection

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.collection.immutable.{LazyList, List, Range}
import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnit4])
class EqualityTest {

  @Test
  def equality(): Unit = {
    val list: Iterable[Int] = List(1, 2, 3)
    val lazyList: Iterable[Int] = LazyList(1, 2, 3)
    val buffer = ArrayBuffer(1, 2, 3)
    val range = Range.inclusive(1, 3)
    Assert.assertTrue(list == lazyList)
    Assert.assertTrue(list.## == lazyList.##)
    Assert.assertTrue(list == (buffer: Iterable[Int]))
    Assert.assertTrue(list.## == buffer.##)
    Assert.assertTrue(list == (range: Iterable[Int]))
    Assert.assertTrue(list.## == range.##)
    buffer += 4
    Assert.assertTrue(list != (buffer: Iterable[Int]))
    Assert.assertTrue(list.## != buffer.##)
  }

  @Test
  def viewsComparisonDoesntConsumeElements(): Unit = {
    val xs = List(1, 2, 3)
    var n = 0
    val xsView = xs.view.map { x => n += 1; x }
    Assert.assertTrue(xs != xsView)
    Assert.assertTrue(xsView != xs)
    Assert.assertTrue(xs.## != xsView.##)
    Assert.assertEquals(0, n) // The view hasn’t been evaluated
    Assert.assertTrue(xs sameElements xsView) // sameElements does evaluate the view elements
    Assert.assertEquals(3, n)
  }

}
