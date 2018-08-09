package scala.collection

import scala.collection.immutable.List

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import language.postfixOps

@RunWith(classOf[JUnit4])
class ViewTest {

  @Test
  def hasCorrectDropAndTakeMethods(): Unit = {
    val iter = Iterable(1, 2, 3)

    import scala.language.postfixOps
    assertEquals(Iterable.empty[Int], iter.view take Int.MinValue to Iterable)
    assertEquals(Iterable.empty[Int], iter.view takeRight Int.MinValue to Iterable)
    assertEquals(iter, iter.view drop Int.MinValue to Iterable)
    assertEquals(iter, iter.view dropRight Int.MinValue to Iterable)
  }

  @Test
  def seqView(): Unit = {
    val xs = List(1, 2, 3).view
    assertEquals(List(3, 2, 1), xs.reverse.to(List))
    assertEquals(2, xs(1))
//    assertEquals(xs, xs.reverse.reverse.to(List)) doesn’t compile
  }

  @Test
  def mapView(): Unit = {
    val xs = immutable.Map(1 -> "a", 2 -> "b")
    assertEquals("a", xs.view(1))
    val ys = xs.view.mapValues(_.toUpperCase)
    assertTrue(ys.contains(1))
    assertEquals("B", ys(2))
  }

  @Test
  def viewsViewIsNoOp(): Unit = {
    def check[A](it: Iterable[A]): Unit = {
      val view = it.view
      assertTrue(view eq view.view)
    }
    check(immutable.Set(1, 2, 3)) // View
    check(List(1, 2, 3)) // SeqView
    check(immutable.Vector(1, 2, 3)) // IndexedSeqView
    check(immutable.Map(1 -> "a", 2 -> "b")) // MapView
  }

}
