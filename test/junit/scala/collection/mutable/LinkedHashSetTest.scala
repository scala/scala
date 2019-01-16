package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.{ Assert, Test }

import scala.collection.mutable

/* Test for scala/bug#9095 */
@RunWith(classOf[JUnit4])
class LinkedHashSetTest {
  class TestClass extends mutable.LinkedHashSet[String] {
    def lastItemRef = lastEntry
  }

  @Test
  def foo: Unit = {
    def go(n: Int): Unit = {
      val lhs = (1 to n).to(LinkedHashSet)

      print(lhs.getTable.table.count(_ eq null))
      print(" ")
      println(lhs.getTable.table.count(_ ne null))
    }

    List(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000).foreach(go)
  }
  
  @Test
  def testClear: Unit = {
    val lhs = new TestClass
    Seq("a", "b").foreach(k => lhs.add(k))
    
    Assert.assertNotNull(lhs.lastItemRef)
    lhs.clear()
    Assert.assertNull(lhs.lastItemRef)
  }
}
