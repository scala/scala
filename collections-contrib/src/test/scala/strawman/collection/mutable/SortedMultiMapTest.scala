package strawman.collection.mutable

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class SortedMultiMapTest {

  @Test
  def sortedMultiMap(): Unit = {
    val smm = SortedMultiMap.empty[String, Int]

    smm += "a" -> 1
    smm += "b" -> 1
    smm += "b" -> 2
    smm += "c" -> 1

    val m = Map("a" -> Set(1), "b" -> Set(1, 2), "c" -> Set(1))
    Assert.assertEquals(m, smm.sets)
    Assert.assertEquals(smm.sets, m)

    Assert.assertEquals("a", smm.firstKey)
    Assert.assertEquals("c", smm.lastKey)

    Assert.assertEquals(SortedMultiMap("c" -> 1), smm.from("c"))

    smm += "a" -> 2
    Assert.assertEquals(Set(1, 2), smm.get("a"))
    smm -= "a" -> 1
    Assert.assertEquals(Set(2), smm.get("a"))
    Assert.assertTrue(smm.containsKey("a"))
    smm -= "a" -> 2
    Assert.assertFalse(smm.containsKey("a"))
    Assert.assertEquals(Set(1, 2), smm.get("b"))
    smm -*= "b"
    Assert.assertEquals(Set.empty, smm.get("b"))
  }

}
