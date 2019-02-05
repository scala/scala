package scala.collection.immutable

import org.junit.Assert.{assertEquals, assertSame}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class HashMapTest {

  @Test
  def canMergeIdenticalHashMaps(): Unit = {
    def m = HashMap(1 -> 1)
    val merged = m.merged(m)(null)
    assertEquals(m, merged)
  }

  @Test
  def canMergeIdenticalHashMapsCustomMerge(): Unit = {
    def m = HashMap(1 -> 1)
    val merged = m.merged(m) {
      case ((k1, v1), (_, v2)) => (k1, v1 + v2)
    }
    assertEquals(HashMap(1 -> 2), merged)
  }

  @Test
  def canMergeHashMapsWithHashCollision(): Unit = {
    val key1 = 1000L * 1000 * 1000 * 10
    val key2 = key1.##.toLong
    assert(key1.## == key2.##)

    val m1 = HashMap(key1 -> 1)
    val m2 = HashMap(key2 -> 1)
    val expected = HashMap(key1 -> 1, key2 -> 1)
    val merged = m1.merged(m2)(null)
    assertEquals(expected, merged)
    val mergedWithMergeFunction = m1.merged(m2) { (kv1, kv2) =>
      throw new RuntimeException("Should not be reached.")
    }
    assertEquals(expected, mergedWithMergeFunction)
  }

  @Test
  def testWithDefaultValue: Unit = {
    val m1 = HashMap(1 -> "a", 2 -> "b")
    val m2 = m1.withDefaultValue(0)
    assertEquals("a", m2(1))
    assertEquals(0, m2(3))
  }

  @Test
  def factoryReuse(): Unit = {
    assertSame(HashMap.empty, HashMap.empty)
    assertSame(HashMap.empty, HashMap())
    val m = HashMap("a" -> "a")
    assertSame(m, HashMap.from(m))
  }

  @Test
  def testGetOrElse: Unit = {
    val m1 = HashMap(1 -> "a", 2 -> "b")
    assertEquals("a", m1.getOrElse(1, ???))
    assertEquals("c", m1.getOrElse(3, "c"))

    class Collider {
      override def hashCode = 0
    }
    val a, b, c = new Collider
    val m2 = HashMap(a -> "a", b -> "b")
    assertEquals("a", m2.getOrElse(a, ???))
    assertEquals("c", m2.getOrElse(c, "c"))
  }

  @Test
  def testWithDefault: Unit = {
    val m1 = HashMap(1 -> "a", 2 -> "b")

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

  @Test
  def canMergeHashMapCollision1WithCorrectMerge() {
    case class A(k: Int) { override def hashCode = 0 }
    val m1 = HashMap(A(0) -> 2, A(1) -> 2)
    val m2 = HashMap(A(0) -> 1, A(1) -> 1)
    val merged = m1.merged(m2) { case ((k, l), (_, r)) => k -> (l - r) }
    val expected = HashMap(A(0) -> 1, A(1) -> 1)
    assertEquals(merged, expected)
  }
  @Test
  def transformReturnsOriginalMap() {
    case class A(i: Int, j: Int) { override def hashCode = j }

    val hashMap = HashMap(
      A(1, 1) -> 1,
      A(1, 2) -> 1,
      A(2, 1) -> 1,
      A(2, 2) -> 1
    )

    assert(hashMap.transform((_, v) => v) eq hashMap)
  }
  @Test
  def testUpdatedWith(): Unit = {
    val hashMap = HashMap(1 -> "a")

    val insertIfAbesent: Option[String] => Option[String] = _.orElse(Some("b"))
    assertEquals(hashMap.updatedWith(1)(insertIfAbesent), HashMap(1 -> "a"))
    assertEquals(hashMap.updatedWith(2)(insertIfAbesent), HashMap(1 -> "a", 2 -> "b"))

    val noneAnytime: Option[String] => Option[String] = _ => None
    assertEquals(hashMap.updatedWith(1)(noneAnytime), HashMap())
    assertEquals(hashMap.updatedWith(2)(noneAnytime), HashMap(1 -> "a"))
  }
}

