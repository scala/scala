package scala.collection

import org.junit.jupiter.api.Test

import scala.{collection => c}
import scala.collection.{immutable => i, mutable => m}

class UnsortedTest {
  @Test
  def usortedIsSpecific(): Unit = {
    c.BitSet(1).unsorted: c.Set[Int]
    c.SortedMap(1 -> 1).unsorted: c.Map[Int, Int]
    c.SortedSet(1).unsorted: c.Set[Int]
    i.BitSet(1).unsorted: i.Set[Int]

    i.SortedMap(1 -> 1).unsorted: i.Map[Int, Int]
    i.SortedSet(1).unsorted: i.Set[Int]
    i.TreeMap(1 -> 1).unsorted: i.Map[Int, Int]
    i.TreeSet(1).unsorted: i.Set[Int]

    m.BitSet(1).unsorted: m.Set[Int]
    m.SortedMap(1 -> 1).unsorted: m.Map[Int, Int]
    m.SortedSet(1).unsorted: m.Set[Int]
    m.TreeMap(1 -> 1).unsorted: m.Map[Int, Int]
    m.TreeSet(1).unsorted: m.Set[Int]
  }
}
