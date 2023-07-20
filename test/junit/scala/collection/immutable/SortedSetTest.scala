package scala.collection.immutable

import org.junit.Test

import scala.annotation.nowarn
import scala.tools.testkit.AllocationTest

class SortedSetTest extends AllocationTest{

  @nowarn("cat=w-flag-value-discard")
  @Test def empty(): Unit ={
    val ord = Ordering[String]
    exactAllocates(24)(SortedSet.empty(ord))
  }
  @nowarn("cat=w-flag-value-discard")
  @Test def apply0(): Unit ={
    val ord = Ordering[String]
    exactAllocates(24)(SortedSet()(ord))
  }
  @nowarn("cat=w-flag-value-discard")
  @Test def apply1(): Unit ={
    val ord = Ordering[String]
    exactAllocates(152)(SortedSet("a")(ord))
  }
  @nowarn("cat=w-flag-value-discard")
  @Test def apply2(): Unit ={
    val ord = Ordering[String]
    exactAllocates(216)(SortedSet("a", "b")(ord))
  }
}
