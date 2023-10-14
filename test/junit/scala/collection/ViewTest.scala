package scala.collection

import org.junit.Assert._
import org.junit.Test

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.tools.testkit.AssertUtil.assertSameElements

class ViewTest {

  @Test
  def hasCorrectDropAndTakeMethods(): Unit = {
    val iter = Iterable(1, 2, 3)

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
  def tapEach(): Unit = {
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

  @Test
  def updated(): Unit = {
    def checkThrows[U](f: => U) = try { f; assertTrue(false) } catch { case _: IndexOutOfBoundsException => }
    // View.Updated can update the last element but not the one after:
    val v1 = new View.Updated(0 until 5, 4, 0)
    val v2 = new View.Updated(0 until 5, 5, 0)
    assertEquals(List(0,1,2,3,0), v1.toList)
    checkThrows(v2.toList)
    // Seq.updated throws immediately for strict collections:
    checkThrows(ArrayBuffer.from(0 until 5).updated(5, 0))
    checkThrows(ArrayBuffer.from(0 until 5).updated(-1, 0))
    // Negative indices result in an immediate exception even for lazy collections:
    checkThrows(LazyList.from(0 until 5).updated(-1, 0))
    // `updated` does not force a LazyList but forcing it afterwards will check the index:
    val ll = LazyList.from(0 until 5).updated(5, 0)
    checkThrows(ll.toList)
  }

  @deprecated("Tests deprecated API", since="2.13")
  @Test
  def `t10103 result of view must be indexed seq`(): Unit = {
    val ints: IndexedSeq[Int] = Vector(1, 2, 3, 4)
    ints.view(1, 3): scala.collection.IndexedSeqView[Int]
  }

  @Test
  def _toString(): Unit = {
    assertEquals("View(<not computed>)", View(1, 2, 3).toString)
  }

  // see scala/scala#9388
  @Test
  def patch(): Unit = {
    // test re-iterability
    val v1 = List(2).view.patch(1, List(3, 4, 5).iterator, 0)
    assertSameElements(Seq(2, 3, 4, 5), v1.toList)
    assertSameElements(Seq(2, 3, 4, 5), v1.toList) // check that it works twice

    // https://github.com/scala/scala/pull/9388#discussion_r709392221
    val v2 = List(2).view.patch(1, Nil, 0)
    assert(!v2.isEmpty)

    // https://github.com/scala/scala/pull/9388#discussion_r709481748
    val v3 = Nil.view.patch(0, List(1).iterator, 0)
    assert(v3.knownSize != 0)
  }
}
