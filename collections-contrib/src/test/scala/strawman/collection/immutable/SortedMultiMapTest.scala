package strawman.collection.immutable

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class SortedMultiMapTest {

  @Test
  def sortedMultiMap(): Unit = {
    val smm = SortedMultiMap("a" -> 1, "b" -> 1, "b" -> 2, "c" -> 1)
    val m = Map("a" -> Set(1), "b" -> Set(1, 2), "c" -> Set(1))
    Assert.assertEquals(m, smm.sets)
    Assert.assertEquals(smm.sets, m)

    Assert.assertEquals("a", smm.firstKey)
    Assert.assertEquals("c", smm.lastKey)

    Assert.assertEquals(SortedMultiMap("c" -> 1), smm.from("c"))

    val smm2 = smm + ("a" -> 2)
    Assert.assertEquals(Set(1, 2), smm2.get("a"))
    val smm3 = smm2 - ("a" -> 1)
    Assert.assertEquals(Set(2), smm3.get("a"))
    Assert.assertTrue(smm3.containsKey("a"))
    val smm4 = smm3 - ("a" -> 2)
    Assert.assertFalse(smm4.containsKey("a"))
    Assert.assertEquals(Set(1, 2), smm4.get("b"))
    val smm5 = smm4 -* "b"
    Assert.assertEquals(Set.empty, smm5.get("b"))
  }

}
