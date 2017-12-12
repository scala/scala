package strawman.collection.immutable

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class MultiMapTest {

  @Test
  def multiMap(): Unit = {
    val mm = MultiMap("a" -> 1, "b" -> 1, "b" -> 2, "c" -> 1)
    val m = Map("a" -> Set(1), "b" -> Set(1, 2), "c" -> Set(1))
    Assert.assertEquals(m, mm.sets)
    Assert.assertEquals(mm.sets, m)

    val mm2 = mm + ("a" -> 2)
    Assert.assertEquals(Set(1, 2), mm2.get("a"))
    val mm3 = mm2 - ("a" -> 1)
    Assert.assertEquals(Set(2), mm3.get("a"))
    Assert.assertTrue(mm3.containsKey("a"))
    val mm4 = mm3 - ("a" -> 2)
    Assert.assertFalse(mm4.containsKey("a"))
    Assert.assertEquals(Set(1, 2), mm4.get("b"))
    val mm5 = mm4 -* "b"
    Assert.assertEquals(Set.empty, mm5.get("b"))
  }

}
