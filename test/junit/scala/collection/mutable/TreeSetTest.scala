package scala.collection.mutable

import org.junit.Assert.assertEquals
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
}
