package scala.collection.mutable

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.immutable.{ List,  Vector}
import scala.collection.{Stepper, immutable, mutable}


@RunWith(classOf[JUnit4])
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

  private def drain[A](s: Stepper[A]): List[A] = {
    val prefix = s.trySplit()
    (if (prefix == null) Nil else prefix.iterator.toList) ++ s.iterator.toList
  }

  // Projections used to inherit whole-tree overrides of minAfter/maxBefore, the stepper and the `from` fast path
  @Test
  def rangeProjectionsRespectBounds(): Unit = {
    val set = mutable.TreeSet(1 to 9: _*)
    val projections = List(
      set.rangeFrom(4)              -> (4 to 9),
      set.rangeUntil(6)             -> (1 to 5),
      set.range(3, 7)               -> (3 to 6),
      set.range(3, 7).rangeFrom(5)  -> (5 to 6),
      set.range(20, 30)             -> (1 until 1),
    )
    for ((proj, elems) <- projections) {
      val expected = elems.toList

      for (k <- 0 to 10) {
        assertEquals(s"minAfter($k)", expected.find(_ >= k), proj.minAfter(k))
        assertEquals(s"maxBefore($k)", expected.findLast(_ < k), proj.maxBefore(k))
      }

      assertEquals(expected, proj.stepper.iterator.toList)
      assertEquals(expected, drain(proj.stepper))

      assertEquals(expected, mutable.TreeSet.from(proj).toList)
      assertEquals(expected, proj.to(mutable.TreeSet).toList)
      assertEquals(expected, mutable.SortedSet.from(proj).toList)
      assertEquals(expected, immutable.TreeSet.from(proj).toList)
    }
  }
}
