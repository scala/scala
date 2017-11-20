package strawman
package collection
package mutable

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class MultiSetTest {

  @Test
  def multiSet(): Unit = {
    val ms = MultiSet.empty[String]

    ms += "a"
    ms += "a"
    ms += "b"

    val m = Map("a" -> 2, "b" -> 1)
    Assert.assertEquals(m, ms.occurrences)

    Assert.assertTrue(ms.contains("a"))
    Assert.assertTrue(ms.contains("b"))
    Assert.assertEquals(2, ms.get("a"))
    Assert.assertEquals(1, ms.get("b"))

    ms -= "b"
    Assert.assertFalse(ms.contains("b"))
    Assert.assertEquals(0, ms.get("b"))

  }

}
