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

    val m2: Map[Int, String] =
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

    val m3: Map[Int, String] = m2 - 1
    assertEquals(m3(1), "2")

    val m4: Map[Int, String] = m3 -- List(2, 100)
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

  @Test
  def testUpdatedWithHashCollisions(): Unit = {
    case class Key(i: Int, j: Int) {
      override def hashCode(): Int = i
    }

    def slowImplementation[K, V](m: Map[K, V])(key: K)(f: Option[V] => Option[V]): Map[K, V] =
      f(m.get(key)) match {
        case Some(v1) => m.updated(key, v1)
        case None => m.removed(key)
      }

    def runTest[K, V](m: Map[K, V])(key: K)(f: Option[V] => Option[V]): Unit = {
      var valuePresented: Option[V] = null
      m.updatedWith(key){ v =>
        valuePresented = v
        f(v)
      }
      assertEquals(m.get(key), valuePresented)
      assertEquals(slowImplementation(m)(key)(f), m.updatedWith(key)(f))
    }

    val map = HashMap(
      // hash collision group 1
      Key(1, 1) -> 1,
      Key(1, 2) -> 2,
      Key(1, 3) -> 3,
      Key(1, 4) -> 4,

      // hash collision group 2
      Key(2, 1) -> 5,
      Key(2, 2) -> 6,
      Key(2, 3) -> 7,
      Key(2, 4) -> 8,

      // other keys
      Key(3, 1) -> 9,
      Key(4, 1) -> 10,
      Key(5, 1) -> 11,
      Key(6, 1) -> 12
    )

    // some to some (same)
    def runSomeToSomeSame(key: Key): Unit = runTest(map)(key)(identity)

    runSomeToSomeSame(Key(1,1))
    runSomeToSomeSame(Key(1,2))
    runSomeToSomeSame(Key(1,3))
    runSomeToSomeSame(Key(1,4))

    runSomeToSomeSame(Key(2,1))
    runSomeToSomeSame(Key(2,2))
    runSomeToSomeSame(Key(2,3))
    runSomeToSomeSame(Key(2,4))

    runSomeToSomeSame(Key(3,1))
    runSomeToSomeSame(Key(4,1))
    runSomeToSomeSame(Key(5,1))
    runSomeToSomeSame(Key(6,1))

    // some to some (different)
    def runSomeToSomeDifferent(key: Key): Unit = {
      runTest(map)(key)(_.map(_ + 1))
    }
    runSomeToSomeDifferent(Key(1,1))
    runSomeToSomeDifferent(Key(1,2))
    runSomeToSomeDifferent(Key(1,3))
    runSomeToSomeDifferent(Key(1,4))

    runSomeToSomeDifferent(Key(2,1))
    runSomeToSomeDifferent(Key(2,2))
    runSomeToSomeDifferent(Key(2,3))
    runSomeToSomeDifferent(Key(2,4))

    runSomeToSomeDifferent(Key(3,1))
    runSomeToSomeDifferent(Key(4,1))
    runSomeToSomeDifferent(Key(5,1))
    runSomeToSomeDifferent(Key(6,1))

    // some to none

    def runSomeToNone(key: Key): Unit = runTest(map)(key)(_ => None)

    runSomeToNone(Key(1,1))
    runSomeToNone(Key(1,2))
    runSomeToNone(Key(1,3))
    runSomeToNone(Key(1,4))

    runSomeToNone(Key(2,1))
    runSomeToNone(Key(2,2))
    runSomeToNone(Key(2,3))
    runSomeToNone(Key(2,4))

    runSomeToNone(Key(3,1))
    runSomeToNone(Key(4,1))
    runSomeToNone(Key(5,1))
    runSomeToNone(Key(6,1))

    // none to some

    def runNoneToSome(key: Key, value: Int): Unit = runTest(map)(key)(_ => Some(value))

    runNoneToSome(Key(1, 5), 6)
    runNoneToSome(Key(1, 6), 7)

    runNoneToSome(Key(2, 5), 6)
    runNoneToSome(Key(2, 6), 7)

    runNoneToSome(Key(3, 2), 6)
    runNoneToSome(Key(7, 1), 6)
    runNoneToSome(Key(8, 1), 7)

    // none to none
    def runNoneToNone(key: Key): Unit = runTest(map)(key)(_ => None)

    runNoneToNone(Key(1, 5))
    runNoneToNone(Key(1, 6))

    runNoneToNone(Key(2, 5))
    runNoneToNone(Key(2, 6))

    runNoneToNone(Key(3, 2))
    runNoneToNone(Key(4, 2))

    runNoneToNone(Key(7, 1))
    runNoneToNone(Key(8, 1))


  }

}
