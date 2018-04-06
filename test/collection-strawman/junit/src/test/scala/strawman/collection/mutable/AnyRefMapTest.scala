package strawman.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

@RunWith(classOf[JUnit4])
class AnyRefMapTest {

  @Test def testAnyRefMapCopy: Unit = {
    val m1 = AnyRefMap("a" -> "b")
    val m2: AnyRefMap[String, AnyRef] = AnyRefMap.from(m1)
    assertEquals(m1, m2)
  }

  @Test def testAnyRefMapContains: Unit = {
    val m = AnyRefMap("a" -> 1)
    assertEquals(1, m.size)
    assertTrue(m.contains("a"))
  }

  @Test
  def test10540: Unit = {
    val badHashCode = -2105619938
    val reported = "K00278:18:H7C2NBBXX:7:1111:7791:21465"
    val equivalent = "JK1C=H"
    val sameHashCode = java.lang.Integer.valueOf(badHashCode)

    assertTrue(AnyRefMap(reported -> 1) contains reported)
    assertTrue(AnyRefMap(equivalent -> 1) contains equivalent)
    assertTrue(AnyRefMap(sameHashCode -> 1) contains sameHashCode)
    assertTrue(sameHashCode.hashCode == badHashCode)  // Make sure test works
  }
}
