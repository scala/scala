package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class TreeMapTest {

  @Test
  def hasCorrectDropAndTakeMethods(): Unit = {
    val tree = TreeMap(1 -> "a", 2 -> "b", 3 -> "c")

    assertEquals(TreeMap.empty[Int, String], tree take Int.MinValue)
    assertEquals(TreeMap.empty[Int, String], tree takeRight Int.MinValue)
    assertEquals(tree, tree drop Int.MinValue)
    assertEquals(tree, tree dropRight Int.MinValue)
  }

  @Test
  def factoryReuse(): Unit = {
    val m = TreeMap("a" -> "a")
    assertSame(m, TreeMap.from(m))
  }
  @Test
  def testWithDefaultValue: Unit = {
    val m1 = TreeMap(1 -> "a", 2 -> "b")
    val m2 = m1.withDefaultValue(0)
    assertEquals("a", m2(1))
    assertEquals(0, m2(3))
  }
  @Test
  def testWithDefault: Unit = {
    val m1 = TreeMap(1 -> "a", 2 -> "b")

    val m2: Map.WithDefault[Int, String] =
      m1.withDefault(i => (i + 1).toString)
        .updated(1, "aa")
        .updated(100, "bb")
        .concat(List(500 -> "c", 501 -> "c"))

    assertEquals(m2(1), "aa")
    assertEquals(m2(2), "b")
    assertEquals(m2(3), "4")
    assertEquals(m2(4), "5")
    assertEquals(m2(500), "c")
    assertEquals(m2(501), "c")
    assertEquals(m2(502), "503")

    val m3: Map.WithDefault[Int, String] = m2 - 1
    assertEquals(m3(1), "2")

    val m4: Map.WithDefault[Int, String] = m3 -- List(2, 100)
    assertEquals(m4(2), "3")
    assertEquals(m4(100), "101")
  }
}
