package strawman.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.{ Assert, Test }

/* Test for scala/bug#9095 */
@RunWith(classOf[JUnit4])
class LinkedHashMapTest {
  class TestClass extends LinkedHashMap[String, Int] {
    def lastItemRef = lastEntry
  }
  
  @Test
  def testClear: Unit = {
    val lhm = new TestClass
    Seq("a" -> 8, "b" -> 9).foreach(kv => lhm.put(kv._1, kv._2))
    
    Assert.assertNotNull(lhm.lastItemRef)
    lhm.clear()
    Assert.assertNull(lhm.lastItemRef)
  }
}
