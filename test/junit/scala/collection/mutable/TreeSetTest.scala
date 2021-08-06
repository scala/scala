package scala.collection.mutable

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

import scala.collection.immutable.{ List,  Vector}
import scala.collection.mutable


class TreeSetTest {

  @Test
  def rangeMkString(): Unit = {

    val set = mutable.TreeSet("a", "b", "c", "d")
    assertEquals("b", set.range("b", "c").mkString(","))
  }

  @deprecated("Tests deprecated API", since="2.13.4")
  @Test def diff(): Unit = {
    val src        = TreeSet(1, 2, 4, 5)
    val removeList = List(1, 2, 7)
    val removeVec  = Vector(1, 2, 7)
    val removeISS  = TreeSet(1, 2, 7)
    val removeMSS  = scala.collection.immutable.TreeSet(1, 2, 7)
    val removeIBS  = BitSet(1, 2, 7)
    val removeMBS  = scala.collection.immutable.BitSet(1, 2, 7)

    val expected = SortedSet(4, 5)
    for (set <- scala.List[collection.Set[Int]](removeISS, removeMSS, removeIBS, removeMBS)) {
      assertEquals(expected, src diff set)
      assertEquals(expected, src &~ set)
      assertEquals(expected, src filterNot set)
    }

    for (set <- scala.List[collection.IterableOnce[Int]](removeList, removeVec, removeVec.iterator, removeISS, removeMSS, removeIBS, removeMBS)) {
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
    for (set <- scala.List[collection.Set[Int]](keepISS, keepMSS, keepIBS, keepMBS)) {
      assertEquals(expected, src intersect set)
      assertEquals(expected, src filter set)
    }
  }
}
