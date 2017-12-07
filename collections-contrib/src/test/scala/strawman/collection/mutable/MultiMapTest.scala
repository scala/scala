package strawman
package collection
package mutable

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class MultiMapTest {

  @Test
  def multiMap(): Unit = {
    val mm = MultiMap.empty[String, Int]

    mm += "a" -> 1
    mm += "a" -> 2
    mm += "b" -> 1

    val m = Map("a" -> Set(1, 2), "b" -> Set(1))
    Assert.assertEquals(m, mm.sets)

    Assert.assertTrue(mm.containsKey("a"))
    Assert.assertTrue(mm.containsEntry("b" -> 1))
    Assert.assertTrue(mm.containsValue(2))
    Assert.assertFalse(mm.containsKey("c"))
    Assert.assertFalse(mm.containsEntry("a" -> 3))
    Assert.assertFalse(mm.containsValue(3))
    Assert.assertEquals(Set(1, 2), mm.get("a"))
    Assert.assertEquals(Set(1), mm.get("b"))

    mm -= "b" -> 1
    Assert.assertFalse(mm.containsKey("b"))
    Assert.assertEquals(Set.empty, mm.get("b"))

    mm -*= "a"
    Assert.assertFalse(mm.containsKey("a"))
    Assert.assertEquals(Set.empty, mm.get("a"))

  }

}
