package strawman.collection.immutable

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class MultiSetTest {

  @Test
  def multiSet(): Unit = {
    val ms = MultiSet("a", "b", "b", "c")
    val m = Map("a" -> 1, "b" -> 2, "c" -> 1)
    Assert.assertEquals(m, ms.occurrences)
    Assert.assertEquals(ms.occurrences, m)

    Assert.assertEquals(1, ms.get("a"))
    Assert.assertEquals(2, ms.get("b"))

    val ms2 = ms + "a"
    Assert.assertEquals(2, ms2.get("a"))
    val ms3 = ms2 - "a"
    Assert.assertEquals(1, ms3.get("a"))
    Assert.assertTrue(ms3.contains("a"))
    val ms4 = ms3 - "a"
    Assert.assertFalse(ms4.contains("a"))
  }

}
