package scala.collection.immutable

import org.junit.Test

import scala.tools.testing.AllocationTest


class SortedMapTest extends AllocationTest{

  @Test def empty: Unit ={
    val ord = Ordering[String]
    exactAllocates(24)(SortedMap.empty[String, String](ord))
  }
  @Test def apply0: Unit ={
    val ord = Ordering[String]
    exactAllocates(24)(SortedMap()(ord))
  }
  @Test def apply1: Unit ={
    val ord = Ordering[String]
    exactAllocates(160)(SortedMap("a" -> "a")(ord))
  }
  @Test def apply2: Unit ={
    val ord = Ordering[String]
    exactAllocates(216)(SortedMap("a" -> "a", "b" -> "b")(ord))
  }
}
