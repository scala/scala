package scala.collection

import org.junit.{Assert, Test}, Assert.assertEquals

class SortedSetMapEqualsTest {
  @Test
  def noOptimizedSetEqualityWhenOrderingsDiffer(): Unit = {
    checkSet(ord => mutable.SortedSet.newBuilder[Int](ord))
    checkSet(ord => immutable.SortedSet.newBuilder[Int](ord))
    checkMap(ord => mutable.SortedMap.newBuilder[Int, Any](ord))
    checkMap(ord => immutable.SortedMap.newBuilder[Int, Any](ord))
  }

  private def checkSet[Coll <: Set[Int]](builder: (Ordering[Int] => mutable.Builder[Int, Coll])): Unit = {
    class ordInary[A](elements: List[A]) extends Ordering[A] {
      def compare(x: A, y: A): Int = elements.indexOf(x) compare elements.indexOf(y)
    }

    val m1 = {
      val ord = new ordInary(List(1, 2, 3))
      val b = builder(ord)
      b += 1
      b += 2
      b += 3
      val m = b.result
      val m1 = m + 4
      m1
    }

    val m2 = {
      val ord = new ordInary(List(1, 2, 3, 4))
      val b = builder(ord)
      b += 1
      b += 2
      b += 3
      b += 4
      val m = b.result
      m
    }
    assertEquals(m1, m2)
  }

  private def checkMap[Coll <: Map[Int, Any]](builder: (Ordering[Int] => mutable.Builder[(Int, Any), Coll])): Unit = {
    class ordInary[A](elements: List[A]) extends Ordering[A] {
      def compare(x: A, y: A): Int = elements.indexOf(x) compare elements.indexOf(y)
    }

    val m1 = {
      val ord = new ordInary(List(1, 2, 3))
      val b = builder(ord)
      b += (1 -> "")
      b += (2 -> "")
      b += (3 -> "")
      val m = b.result
      val m1 = m + (4 -> "")
      m1
    }

    val m2 = {
      val ord = new ordInary(List(1, 2, 3, 4))
      val b = builder(ord)
      b += (1 -> "")
      b += (2 -> "")
      b += (3 -> "")
      b += (4 -> "")
      val m = b.result
      m
    }
    assertEquals(m1, m2)
  }
}
