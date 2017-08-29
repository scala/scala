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
}
