package scala.collection

import scala.collection.immutable.List
import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import language.postfixOps
import scala.collection.mutable.ListBuffer

@RunWith(classOf[JUnit4])
class ViewTest {

  @Test
  def hasCorrectDropAndTakeMethods(): Unit = {
    val iter = Iterable(1, 2, 3)

    import scala.language.postfixOps
    assertEquals(Iterable.empty[Int], iter.view take Int.MinValue to Iterable)
    assertEquals(Iterable.empty[Int],
                 iter.view takeRight Int.MinValue to Iterable)
    assertEquals(iter, iter.view drop Int.MinValue to Iterable)
    assertEquals(iter, iter.view dropRight Int.MinValue to Iterable)
  }

  @Test
  def seqView(): Unit = {
    val xs = List(1, 2, 3)
    assertEquals(xs.reverse, xs.view.reverse.toSeq)
    assertEquals(2, xs.view(1))
    val v1 = xs.view.reverse.reverse
    val v1t: SeqView[Int] = v1
    assertEquals(xs, v1t.toSeq)
    val v2 = xs.view.concat(xs)
    val v2t: SeqView[Int] = v2
    assertEquals(xs.concat(xs), v2t.toSeq)
  }

  @Test
  def indexedSeqView(): Unit = {
    val xs = Vector(1, 2, 3)
    assertEquals(xs.reverse, xs.view.reverse.toSeq)
    assertEquals(2, xs.view(1))
    val v1 = xs.view.reverse.reverse
    val v1t: IndexedSeqView[Int] = v1
    assertEquals(xs, v1t.toSeq)
    val v2 = xs.view.concat(xs)
    val v2t: IndexedSeqView[Int] = v2
    assertEquals(xs.concat(xs), v2t.toSeq)
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

  @Test
  def tapEach: Unit = {
    val lb = ListBuffer[Int]()

    val v =
      View(1, 2, 3)
        .tapEach(lb += _)
        .map(_ => 10)
        .tapEach(lb += _)
        .tapEach(_ => lb += -1)

    assertEquals(ListBuffer[Int](), lb)

    val strict = v.to(Seq)
    assertEquals(strict, Seq(10, 10, 10))
    assertEquals(lb, Seq(1, 10, -1, 2, 10, -1, 3, 10, -1))
  }

}
