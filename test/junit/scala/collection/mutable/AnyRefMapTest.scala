package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

import scala.collection.immutable


/* Test for scala/bug#10540 */
@RunWith(classOf[JUnit4])
@annotation.nowarn("cat=deprecation&origin=scala.collection.mutable.AnyRefMap")
class AnyRefMapTest {
  @Test def t13048(): Unit = {
    def t(x: AnyRef, y: AnyRef): Unit = {
      val m = AnyRefMap.empty[AnyRef, Unit]
      m.getOrElseUpdate(x, m.getOrElseUpdate(y, ()))
      assert(m.keys.toSet == immutable.Set(x, y))
    }
    val o1 = new AnyRef {
      override val hashCode = 4422
    }
    val o2 = new AnyRef {
      override val hashCode = 4422
    }
    t(o1, o2)
    t(o2, o1)
    t(o1, o1)
  }

  @Test def testAnyRefMapCopy(): Unit = {
    val m1 = AnyRefMap("a" -> "b")
    val m2: AnyRefMap[String, AnyRef] = AnyRefMap.from(m1)
    assertEquals(m1, m2)
  }

  @Test def testAnyRefMapContains(): Unit = {
    val m = AnyRefMap("a" -> 1)
    assertEquals(1, m.size)
    assertTrue(m.contains("a"))
  }

  @Test
  def test10540(): Unit = {
    val badHashCode = -2105619938
    val reported = "K00278:18:H7C2NBBXX:7:1111:7791:21465"
    val equivalent = "JK1C=H"
    val sameHashCode = java.lang.Integer.valueOf(badHashCode)
    assertTrue(AnyRefMap(reported -> 1) contains reported)
    assertTrue(AnyRefMap(equivalent -> 1) contains equivalent)
    assertTrue(AnyRefMap(sameHashCode -> 1) contains sameHashCode)
    assertTrue(sameHashCode.hashCode == badHashCode)  // Make sure test works
  }

  @deprecated("Tests deprecated API", since="2.13")
  @Test
  def t10876(): Unit = {
    val m = collection.mutable.AnyRefMap("fish" -> 3)
    val m2 = m + (("birds", 2))
    assertEquals(Map("fish" -> 3, "birds" -> 2), (m2: collection.mutable.AnyRefMap[String, Int]))
  }

  @Test
  def testClear(): Unit = {
    val map = new AnyRefMap[String, String]()
    map("greeting") = "hi"
    map("farewell") = "bye"
    assertEquals(2, map.size)
    map.clear()
    assertEquals(0, map.size)
    map("greeting") = "howdy"
    assertEquals(1, map.size)
    map("farewell") = "auf Wiedersehen"
    map("good day") = "labdien"
    assertEquals(3, map.size)
  }

  @Test
  def testClearMemoryReuse(): Unit = { // otherwise there's no point to the override
    val map = new AnyRefMap[String, Int]
    def getField[T <: AnyRef](name: String): T =
      reflect.ensureAccessible(map.getClass.getDeclaredField("scala$collection$mutable$AnyRefMap$$" + name)).get(map).asInstanceOf[T]
    def hashesSz = getField[Array[Int]]("_hashes").length
    def keysSz = getField[Array[AnyRef]]("_keys").length
    def valuesSz = getField[Array[AnyRef]]("_values").length
    def assertArraysSize(sz: Int) = {
      assertEquals(sz, hashesSz)
      assertEquals(sz, keysSz)
      assertEquals(sz, valuesSz)
    }
    for (i <- (1 to 1000000)) map(i.toString) = i
    assertEquals(1000000, map.size)
    assertArraysSize(1 << 21)
    map.clear()
    assertEquals(0, map.size)
    assertArraysSize(1 << 21)
    for (i <- (-10000 to 10000)) map(i.toString) = i
    assertEquals(20001, map.size)
    assertArraysSize(1 << 21)
  }
}
