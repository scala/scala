package strawman.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.{ Assert, Test }

/* Test for scala/bug#9095 */
@RunWith(classOf[JUnit4])
class LinkedHashSetTest {
  class TestClass extends LinkedHashSet[String] {
    def lastItemRef = lastEntry
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
