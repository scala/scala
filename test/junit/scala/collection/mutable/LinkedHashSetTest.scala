package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.{Assert, Test}

import scala.annotation.nowarn
import scala.collection.mutable

/* Test for scala/bug#9095 */
@RunWith(classOf[JUnit4])
class LinkedHashSetTest {
  @nowarn("msg=inheritance from class LinkedHashSet")
  class TestClass extends mutable.LinkedHashSet[String] {
    def lastItemRef = lastEntry
  }
  
  @Test
  def testClear(): Unit = {
    val lhs = new TestClass
    Seq("a", "b").foreach(k => lhs.add(k))
    
    Assert.assertNotNull(lhs.lastItemRef)
    lhs.clear()
    Assert.assertNull(lhs.lastItemRef)
  }
}
