package scala.collection

import org.junit.{Assert, Test}
import Assert.{assertEquals, assertNotEquals}

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
      val m = b.result()
      val res = m.union(Set(4))
      res
    }

    val m2 = {
      val ord = new ordInary(List(1, 2, 3, 4))
      val b = builder(ord)
      b += 1
      b += 2
      b += 3
      b += 4
      val m = b.result()
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
      val m = b.result()
      val res = m.concat(List(4 -> ""))
      res
    }

    val m2 = {
      val ord = new ordInary(List(1, 2, 3, 4))
      val b = builder(ord)
      b += (1 -> "")
      b += (2 -> "")
      b += (3 -> "")
      b += (4 -> "")
      val m = b.result()
      m
    }
    assertEquals(m1, m2)
  }

  @Test
  def compareSortedMapKeysByOrdering(): Unit = {
    val ord: Ordering[String] = _ compareToIgnoreCase _

    val itm1 = scala.collection.immutable.TreeMap("A" -> "2")(ord)
    val itm2 = scala.collection.immutable.TreeMap("a" -> "2")(ord)
    val mtm1 = scala.collection.mutable.TreeMap("A" -> "2")(ord)
    val mtm2 = scala.collection.mutable.TreeMap("a" -> "2")(ord)

    assertEquals(itm1, itm2)
    assertEquals(mtm1, mtm2)

    assertEquals(itm1, mtm2)
    assertEquals(mtm1, itm2)

    val m1 = Map("A" -> "2")
    val m2 = Map("a" -> "2")

    for (m <- List(m1, m2); tm <- List[Map[String, String]](itm1, itm2, mtm1, mtm2))
      assertEquals(m, tm) // uses keys in `m` to look up values in `tm`, which always succeeds

    assertEquals(itm1, m1)
    assertEquals(mtm1, m1)

    assertNotEquals(itm2, m1) // uses key in `itm2` ("a") to look up in `m1`, which fails
    assertNotEquals(mtm2, m1)
  }

  @Test
  def compareSortedSetsByOrdering(): Unit = {
    val ord: Ordering[String] = _ compareToIgnoreCase _

    val its1 = scala.collection.immutable.TreeSet("A")(ord)
    val its2 = scala.collection.immutable.TreeSet("a")(ord)
    val mts1 = scala.collection.mutable.TreeSet("A")(ord)
    val mts2 = scala.collection.mutable.TreeSet("a")(ord)

    assertEquals(its1, its2)
    assertEquals(mts1, mts2)

    assertEquals(its1, mts2)
    assertEquals(mts1, its2)

    val s1 = Set("A")
    val s2 = Set("a")

    for (m <- List(s1, s2); tm <- List[Set[String]](its1, its2, mts1, mts2))
      assertEquals(m, tm) // uses keys in `m` to look up values in `tm`, which always succeeds

    assertEquals(its1, s1)
    assertEquals(mts1, s1)

    assertNotEquals(its2, s1) // uses key in `its2` ("a") to look up in `s1`, which fails
    assertNotEquals(mts2, s1)
  }
}
