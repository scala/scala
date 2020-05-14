package scala.collection.immutable

import java.util
import java.util.Collections

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class TreeSetTest {

  @Test
  def hasCorrectDropAndTakeMethods(): Unit = {
    val set = TreeSet(1, 2, 3)

    assertEquals(TreeSet.empty[Int], set take Int.MinValue)
    assertEquals(TreeSet.empty[Int], set takeRight Int.MinValue)
    assertEquals(set, set drop Int.MinValue)
    assertEquals(set, set dropRight Int.MinValue)
  }

  @Test
  def factoryReuse(): Unit = {
    val m = TreeSet("a")
    assertSame(m, TreeSet.from(m))
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

  @Test
  def t11637: Unit = {
    import scala.collection.immutable.{RedBlackTree => RB}
    val instrs = List[RB.Tree[Int, Null] => RB.Tree[Int, Null]](
      RB.update(_, 18, null, overwrite = false),
      RB.update(_, 0, null, overwrite = false),
      RB.update(_, 3, null, overwrite = false),
      RB.update(_, 4, null, overwrite = false),
      RB.update(_, 1, null, overwrite = false),
      RB.update(_, 11, null, overwrite = false),
      RB.update(_, 14, null, overwrite = false),
      RB.update(_, -1, null, overwrite = false),
      RB.update(_, 2, null, overwrite = false),
      RB.update(_, 17, null, overwrite = false),
      RB.delete(_, 14),
      RB.update(_, 9, null, overwrite = false),
      RB.update(_, 15, null, overwrite = false),
      RB.take(_, 10),
      RB.delete(_, 0),
      RB.update(_, 16, null, overwrite = false),
      RB.take(_, 10),
      RB.delete(_, 11),
      RB.update(_, 11, null, overwrite = false),
      RB.take(_, 10),
      RB.delete(_, 1),
      RB.update(_, 12, null, overwrite = false),
      RB.take(_, 10),
      RB.delete(_, 4),
      RB.update(_, 0, null, overwrite = false),
      RB.take(_, 8),
      RB.delete(_, 11),
    )
    var t: RB.Tree[Int, Null] = null
    instrs.zipWithIndex.foreach { case (instr, idx) =>
      t = instr(t)
    }
  }


  @Test def keysEqualSimple: Unit = {
    val tree = TreeMap(1 -> "a", 2 -> "b", 3 -> "c")
    assertEquals(Set(1,2,3), tree.keySet)
    val tree2 = TreeMap(1 -> "x", 2 -> "y", 3 -> "z")
    assertEquals(tree2.keySet, tree.keySet)
    val tree3 = TreeSet(1,2,3)
    assertEquals(tree3, tree.keySet)
    assertEquals(tree.keySet, tree3)
  }
  @Test def valuesEqual: Unit = {
    val data = (Array.tabulate(1000) { i => i.toString}).sorted
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
    for ( i <- 10 to 1000) {
      b1 += s"$i" -> new V(s"$i value")
    }
    val tree1 = b1.result()

    assertEquals(tree1.drop(5).keySet, tree1.drop(5).keySet)
  }
  @Test def equalFastPath: Unit = {
    class K(private val s: String) extends Comparable[K]{

      override def equals(obj: Any): Boolean = {
        fail("equals should not be called = the trees should be  the same")
        false
      }

      override def compareTo(o: K): Int = s.compareTo(o.s)
    }
    val b1 = TreeSet.newBuilder[K]
    for ( i <- 10 to 1000) {
      b1 += new K(s"$i **")
    }
    val tree1 = b1.result()

    assertEquals(tree1.drop(5), tree1.drop(5))
  }

}
