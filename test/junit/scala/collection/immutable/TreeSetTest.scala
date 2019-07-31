package scala.collection.immutable

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
}
