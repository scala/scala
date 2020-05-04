package scala.collection.immutable

import org.junit.Test

import scala.tools.testing.AllocationTest


class SortedSetTest extends AllocationTest{

  @Test def empty: Unit ={
    val ord = Ordering[String]
    exactAllocates(24)(SortedSet.empty(ord))
  }
  @Test def apply0: Unit ={
    val ord = Ordering[String]
    exactAllocates(48)(SortedSet()(ord))
  }
  @Test def apply1: Unit ={
    val ord = Ordering[String]
    exactAllocates(192)(SortedSet("a")(ord))
  }
  @Test def apply2: Unit ={
    val ord = Ordering[String]
    exactAllocates(280)(SortedSet("a", "b")(ord))
  }
}
