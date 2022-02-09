package scala.collection.mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.immutable.{ List,  Vector}
import scala.collection.mutable


@RunWith(classOf[JUnit4])
class TreeSetTest {

  @Test
  def rangeMkString() {

    val set = mutable.TreeSet("a", "b", "c", "d")
    assertEquals("b", set.range("b", "c").mkString(","))
  }

  @Test def diff(): Unit = {
    val src        = TreeSet(1, 2, 4, 5)
    val removeList = List(1, 2, 7)
    val removeVec  = Vector(1, 2, 7)
    val removeISS  = TreeSet(1, 2, 7)
    val removeMSS  = scala.collection.immutable.TreeSet(1, 2, 7)
    val removeIBS  = BitSet(1, 2, 7)
    val removeMBS  = scala.collection.immutable.BitSet(1, 2, 7)

    val expected = SortedSet(4, 5)
    for (set <- List(removeISS, removeMSS, removeIBS, removeMBS)) {
      assertEquals(expected, src diff set)
      assertEquals(expected, src &~ set)
      assertEquals(expected, src filterNot set)
    }

    for (set <- List(removeList, removeVec, removeVec.iterator, removeISS, removeMSS, removeIBS, removeMBS)) {
      assertEquals(expected, src -- set)
    }
  }
  @Test def intersect(): Unit = {
    val src        = TreeSet(1, 2, 4, 5)
    val keepISS  = TreeSet(1, 2, 7)
    val keepMSS  = scala.collection.immutable.TreeSet(1, 2, 7)
    val keepIBS  = BitSet(1, 2, 7)
    val keepMBS  = scala.collection.immutable.BitSet(1, 2, 7)

    val expected = SortedSet(1, 2)
    for (set <- List(keepISS, keepMSS, keepIBS, keepMBS)) {
      assertEquals(expected, src intersect set)
      assertEquals(expected, src filter set)
    }
  }

  @Test
  def min(): Unit = {
    val nonEmptySet = mutable.TreeSet(2, 4, -1, 3)
    assertEquals(-1, nonEmptySet.min)
    assertEquals(4, nonEmptySet.min(implicitly[Ordering[Int]].reverse))

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
    val nonEmptySet = mutable.TreeSet(2, 4, -1, 3)
    assertEquals(4, nonEmptySet.max)
    assertEquals(-1, nonEmptySet.max(implicitly[Ordering[Int]].reverse))

    try {
      mutable.TreeSet.empty[Int].max
      fail("expect UnsupportedOperationException")
    } catch {
      case e: UnsupportedOperationException =>
        assertEquals("empty.max", e.getMessage)
    }
  }

}
