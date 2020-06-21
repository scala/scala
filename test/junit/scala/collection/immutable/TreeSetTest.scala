package scala.collection.immutable

import java.util
import java.util.Collections

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AllocationTest

@RunWith(classOf[JUnit4])
class TreeSetTest extends AllocationTest{

  @Test
  def hasCorrectDropAndTakeMethods() {
    val set = TreeSet(1, 2, 3)

    assertEquals(TreeSet.empty[Int], set take Int.MinValue)
    assertEquals(TreeSet.empty[Int], set takeRight Int.MinValue)
    assertEquals(set, set drop Int.MinValue)
    assertEquals(set, set dropRight Int.MinValue)
  }

  @Test
  def min(): Unit = {
    assertEquals(1, TreeSet(1, 2, 3).min)
    assertEquals(3, TreeSet(1, 2, 3).min(implicitly[Ordering[Int]].reverse))

    try {
      TreeSet.empty[Int].min
      fail("expect UnsupportedOperationException")
    } catch {
      case e: UnsupportedOperationException =>
        assertEquals("empty.min", e.getMessage)
    }
  }

  @Test
  def max(): Unit = {
    assertEquals(3, TreeSet(1, 2, 3).max)
    assertEquals(1, TreeSet(1, 2, 3).max(implicitly[Ordering[Int]].reverse))

    try {
      TreeSet.empty[Int].max
      fail("expect UnsupportedOperationException")
    } catch {
      case e: UnsupportedOperationException =>
        assertEquals("empty.max", e.getMessage)
    }
  }


  @Test def keysEqualSimple: Unit = {
    val tree = TreeMap(1 -> "a", 2 -> "b", 3 -> "c")
    assertEquals(Set(1, 2, 3), tree.keySet)
    val tree2 = TreeMap(1 -> "x", 2 -> "y", 3 -> "z")
    assertEquals(tree2.keySet, tree.keySet)
    val tree3 = TreeSet(1, 2, 3)
    assertEquals(tree3, tree.keySet)
    assertEquals(tree.keySet, tree3)
  }

  @Test def valuesEqual: Unit = {
    val data = (Array.tabulate(1000) { i => i.toString }).sorted
    val expectedData = data.drop(30)

    Collections.shuffle(util.Arrays.asList(data))

    val tree1 = (TreeSet.newBuilder[String] ++= expectedData).result()
    val tree2 = (TreeSet.newBuilder[String] ++= data).result().drop(30)

    assertEquals(tree1, tree1)
    assertEquals(tree1, tree2)
  }

  @Test def keysFromMapEqualFastPath: Unit = {
    class V(s: String) {

      override def equals(obj: Any): Boolean = {
        fail("equals should not be called = the trees should be  the same")
        false
      }
    }
    val b1 = TreeMap.newBuilder[String, V]
    for (i <- 10 to 1000) {
      b1 += s"$i" -> new V(s"$i value")
    }
    val tree1 = b1.result()

    assertEquals(tree1.drop(5).keySet, tree1.drop(5).keySet)
  }

  @Test def equalFastPath: Unit = {
    var compareCount = 0
    class K(val s: String) extends Ordered[K] {
      override def toString: String = s"K-$s"

      override def compare(that: K): Int = {
        val res = s.compareTo(that.s)
        compareCount += 1
        res
      }

      override def equals(obj: Any): Boolean = {
        fail("equals should not be called = the trees should be ordered and compared via the sort order")
        false
      }
    }
    val b1 = TreeSet.newBuilder[K]
    for ( i <- 10 to 1000) {
      b1 += new K(i.toString)
    }
    val tree1 = b1.result()
    compareCount = 0
    nonAllocating(assertEquals(tree1, tree1))
    assertEquals(0, compareCount)

    var exp = tree1.drop(5)
    var act = tree1.drop(5)
    compareCount = 0
    onlyAllocates(240)(assertEquals(exp, act))
    assertEquals(0, compareCount)

    exp += new K("XXX")
    act += new K("XXX")

    compareCount = 0
    assertEquals(exp, act)
    assertTrue(compareCount.toString, compareCount < 30)
    //we cant combine this with th above assertion as onlyAllocates run lots of time to determne the allocation
    onlyAllocates(408)(assertEquals(exp, act))
  }

  @Test
  def plusWithContains() {
    val data = Array.tabulate(1000)(i => s"${i}Value")
    val tree = (TreeSet.newBuilder[String] ++= data).result

    data foreach {
      case (k) =>
        assertSame(tree, nonAllocating(tree + k))
    }
  }

  @Test
  def plusWithContainsFromMap() {
    val data = Array.tabulate(1000)(i => s"${i}Key" -> s"${i}Value")
    val tree = (TreeMap.newBuilder[String, String] ++= data).result.keySet

    data foreach {
      case (k, v) =>
        assertSame(tree, nonAllocating(tree + k))
    }
  }

  @Test def consistentEquals: Unit = {
    class K(val s: String) extends Ordered[K] {
      override def toString: String = s"K-$s"

      override def compare(that: K): Int = {
        fail("compare should not be called  - should be handled by the custom ordering")
        0
      }
      override def hashCode(): Int = s.hashCode

      override def equals(obj: Any): Boolean = obj match {
        case k:K => k.s == this.s
        case _ => false
      }
    }
    class CustomOrder(val selfEqual: Boolean) extends Ordering[K] {
      override def compare(x: K, y: K): Int = x.s compareTo y.s

      override def equals(obj: Any): Boolean = obj match {
        case c: CustomOrder => (c eq this) || this.selfEqual && c.selfEqual
        case _ => false
      }
    }
    val o1 = new CustomOrder(true)
    val o2_1 = new CustomOrder(false)
    val o2_2 = new CustomOrder(false)

    val b1_1 = TreeSet.newBuilder[K](o1)
    val b1_2 = TreeSet.newBuilder[K](o1)

    val b2_1 = TreeSet.newBuilder[K](o2_1)
    val b2_2 = TreeSet.newBuilder[K](o2_2)

    val bHash = HashSet.newBuilder[K]
    for (i <- 1 to 10) {
      b1_1 += new K(i.toString)
      b1_2 += new K(i.toString)

      b2_1 += new K(i.toString)
      b2_2 += new K(i.toString)

      bHash += new K(i.toString)
    }
    val tree1_1 = b1_1.result()
    val tree1_2 = b1_2.result()

    val tree2_1 = b1_1.result()
    val tree2_2 = b1_2.result()

    val treeHash = bHash.result()

    val all = List((tree1_1, "tree1_1"), (tree1_2, "tree1_2"), (tree2_1, "tree2_1"), (tree2_2, "tree2_2"), (treeHash, "treeHash"))
    for ((lhs, lText) <- all;
         (rhs, rText) <- all) {
      assertEquals(s"$lText $rText", lhs, rhs)
      assertEquals(s"$rText $lText", rhs, lhs)
    }
  }

  @Test
  def unionAndIntersectRetainLeft(): Unit = {
    case class C(a: Int)(override val toString: String)
    implicit val ordering: Ordering[C] = Ordering.by(_.a)
    val c0l = C(0)("l")
    val c0r = C(0)("r")
    def assertIdenticalElements(expected: Set[C], actual: Set[C]): Unit = {
      val expected1, actual1 = Collections.newSetFromMap[C](new java.util.IdentityHashMap())
      expected.foreach(expected1.add)
      actual.foreach(actual1.add)
      assertEquals(expected1, actual1)
    }
    assertIdenticalElements(Set(c0l), HashSet(c0l).union(HashSet(c0r)))
    assertIdenticalElements(Set(c0l), TreeSet(c0l).union(HashSet(c0r)))
    assertIdenticalElements(Set(c0l), TreeSet(c0l).union(TreeSet(c0r)))

    assertIdenticalElements(Set(c0l), HashSet(c0l).+(c0r))
    assertIdenticalElements(Set(c0l), TreeSet(c0l).+(c0r))
    assertIdenticalElements(Set(c0l), TreeSet(c0l).+(c0r))

    assertIdenticalElements(Set(c0l), HashSet(c0l).++(HashSet(c0r)))
    assertIdenticalElements(Set(c0l), TreeSet(c0l).++(HashSet(c0r)))
    assertIdenticalElements(Set(c0l), TreeSet(c0l).++(TreeSet(c0r)))

    assertIdenticalElements(Set(c0l), HashSet.newBuilder[C].++=(HashSet(c0l)).++=(HashSet(c0r)).result())
    assertIdenticalElements(Set(c0l), TreeSet.newBuilder[C].++=(TreeSet(c0l)).++=(HashSet(c0r)).result())
    assertIdenticalElements(Set(c0l), TreeSet.newBuilder[C].++=(TreeSet(c0l)).++=(TreeSet(c0r)).result())

    assertIdenticalElements(Set(c0l), HashSet(c0l).intersect(HashSet(c0r)))
    assertIdenticalElements(Set(c0l), TreeSet(c0l).intersect(HashSet(c0r)))
    assertIdenticalElements(Set(c0l), TreeSet(c0l).intersect(TreeSet(c0r)))
  }
}
