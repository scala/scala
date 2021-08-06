package scala.collection.mutable

import org.junit.jupiter.api.{Assertions, Test}

import scala.collection.mutable

/* Test for scala/bug#9095 */
class LinkedHashSetTest {
  class TestClass extends mutable.LinkedHashSet[String] {
    def lastItemRef = lastEntry
  }
  
  @Test
  def testClear(): Unit = {
    val lhs = new TestClass
    Seq("a", "b").foreach(k => lhs.add(k))

    Assertions.assertNotNull(lhs.lastItemRef)
    lhs.clear()
    Assertions.assertNull(lhs.lastItemRef)
  }
}
