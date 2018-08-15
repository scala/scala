package scala.collection.immutable

import org.junit.Assert.{assertEquals, assertFalse, assertSame, assertTrue}
import org.junit.{Assert, Test}

object ChampMapSmokeTest {

  private def emptyMap[K, V]: HashMap[K, V] =
    HashMap.empty[K, V]

  private def mapOf[K, V](keyValuePairs: (K, V)*): HashMap[K, V] = {
    val builder = HashMap.newBuilder[K, V]
    keyValuePairs.foreach(builder.addOne)
    builder.result()
  }

  def mkTuple[KV](keyValue: KV): (KV, KV) = keyValue -> keyValue

  def mkTuple[K, V](key: K, value: V): (K, V) = key -> value

  def mkValue(value: Int) = new CustomHashInt(value, value)

  def mkValue(value: Int, hash: Int) = new CustomHashInt(value, hash)

}

class ChampMapSmokeTest {

  import ChampMapSmokeTest._

  val v11h1 = mkValue(11, 1)
  val v12h1 = mkValue(12, 1)

  val v1h1 = mkValue(1, 1)
  val v5h5 = mkValue(5, 5)

  val v32769 = mkValue(32769, 32769)
  val v32769a = mkValue(32769*10+1, 32769)
  val v32769b = mkValue(32769*10+2, 32769)

  @Test def testCheckPrefixConstruction(): Unit = {
    val map: HashMap[Int, Int] = emptyMap

    val res1 = map + mkTuple(63) + mkTuple(64) + mkTuple(32768) + mkTuple(2147483647) + mkTuple(65536)
    assert(res1.contains(63))
    assert(res1.contains(64))
    assert(res1.contains(32768))
    assert(res1.contains(65536))
    assert(res1.contains(2147483647))

    val res2 = map + mkTuple(2147483647) + mkTuple(32768) + mkTuple(63) + mkTuple(64) + mkTuple(65536)
    assert(res2.contains(63))
    assert(res2.contains(64))
    assert(res2.contains(32768))
    assert(res2.contains(65536))
    assert(res2.contains(2147483647))

    assert(res1 == res2)
  }

  @Test def testCheckCompactionFromBeginUponDelete(): Unit = {
    val map: HashMap[Int, Int] = emptyMap
    val res1 = map + mkTuple(1) + mkTuple(2)
    val res2 = res1 + mkTuple(32769) - 2
    /* should trigger assertion in data structure if not compacting */
  }

  @Test def testCheckCompactionFromMiddleUponDelete(): Unit = {
    val map: HashMap[Int, Int] = emptyMap
    val res1 = map + mkTuple(1) + mkTuple(2) + mkTuple(65) + mkTuple(66)
    val res2 = res1 + mkTuple(32769) - 66
    assert(!(res1 == res2))
  }

  @Test def testCheckCompactionFromBeginUponDelete_HashCollisionNode1(): Unit = {
    val map: HashMap[CustomHashInt, CustomHashInt] = emptyMap

    val res1 = map + mkTuple(v11h1) + mkTuple(v12h1)
    assertTrue(res1.contains(v11h1))
    assertTrue(res1.contains(v12h1))

    val res2 = res1 - v12h1
    assertTrue(res2.contains(v11h1))
    assertEquals(mapOf() + mkTuple(v11h1), res2)

    val res3 = res1 - v11h1
    assertTrue(res3.contains(v12h1))
    assertEquals(mapOf() + mkTuple(v12h1, v12h1), res3)

    val resX = res1 + mkTuple(v32769) - v12h1
    assertTrue(resX.contains(v11h1))
    assertTrue(resX.contains(v32769))
    assert(!(res1 == resX))
  }

  @Test def testCheckCompactionFromBeginUponDelete_HashCollisionNode2(): Unit = {
    val map: HashMap[CustomHashInt, CustomHashInt] = emptyMap

    val res1 = map + mkTuple(v32769a) + mkTuple(v32769b)
    assertEquals(2, res1.size)
    assertTrue(res1.contains(v32769a))
    assertTrue(res1.contains(v32769b))

    val res2 = res1 + mkTuple(v1h1)
    assertEquals(3, res2.size)
    assertTrue(res2.contains(v1h1))
    assertTrue(res2.contains(v32769a))
    assertTrue(res2.contains(v32769b))

    val res3 = res2 - v32769b
    assertEquals(2, res3.size)
    assertTrue(res3.contains(v1h1))
    assertTrue(res3.contains(v32769a))
    println(scala.runtime.ScalaRunTime.getClass.getProtectionDomain.getCodeSource)
    val expected = mapOf(mkTuple(v1h1), mkTuple(v32769a))
    assertEquals(expected, res3)
  }

  @Test def testCheckCompactionFromBeginUponDelete_HashCollisionNode3(): Unit = {
    val map: HashMap[CustomHashInt, CustomHashInt] = emptyMap
    val res1 = map + mkTuple(v32769a) + mkTuple(v32769b)
    assertEquals(2, res1.size)
    assertTrue(res1.contains(v32769a))
    assertTrue(res1.contains(v32769b))

    val res2 = res1 + mkTuple(v1h1)
    assertEquals(3, res2.size)
    assertTrue(res2.contains(v1h1))
    assertTrue(res2.contains(v32769a))
    assertTrue(res2.contains(v32769b))

    val res3 = res2 - v1h1
    assertEquals(2, res3.size)
    assertTrue(res3.contains(v32769a))
    assertTrue(res3.contains(v32769b))
    assertEquals(res1, res3)
  }

  @Test def testCheckCompactionFromBeginUponDelete_HashCollisionNode4(): Unit = {
    val map: HashMap[CustomHashInt, CustomHashInt] = emptyMap
    val res1 = map + mkTuple(v32769a) + mkTuple(v32769b)
    assertEquals(2, res1.size)
    assertTrue(res1.contains(v32769a))
    assertTrue(res1.contains(v32769b))

    val res2 = res1 + mkTuple(v5h5)
    assertEquals(3, res2.size)
    assertTrue(res2.contains(v5h5))
    assertTrue(res2.contains(v32769a))
    assertTrue(res2.contains(v32769b))

    val res3 = res2 - v5h5
    assertEquals(2, res3.size)
    assertTrue(res3.contains(v32769a))
    assertTrue(res3.contains(v32769b))
    assertEquals(res1, res3)
  }

  @Test def testCreateSingletonWithFactoryMethod(): Unit = {
    val map: HashMap[Int, Int] = emptyMap + mkTuple(63, 65)
    assertTrue(map.contains(63))
    assertEquals(65, map.get(63).get)
  }

  @Test def testRemoveFromSingleton(): Unit = {
    val map: HashMap[Int, Int] = emptyMap + mkTuple(63, 65)
    val res = map - 63
    assertTrue(res.isEmpty)
    assertFalse(res.contains(63))
    assertEquals(emptyMap, res)
  }

  object O1 { override def hashCode = 1 ; override def toString = "O1"}
  class C(val i: Int) { override def hashCode = i % 4 ; override def toString = s"C($i)" }
  val cs = Array.tabulate(4096)(new C(_))

  private def assertSameEqHash(expected: HashMap[Any, Any], actual: HashMap[Any, Any]) = {
    assertEquals(List.from(actual).size, actual.size)
    assertEquals(expected.size, actual.size)
    assertEquals(cachedJavaKeySetHashCode(expected), cachedJavaKeySetHashCode(actual))
    assertEquals(expected.hashCode(), actual.hashCode())
  }

  lazy val (mirror, field) = {
    import scala.reflect.runtime.{universe=>ru}
    val mirror = ru.runtimeMirror(classOf[HashMap[_,_]].getClassLoader)
    val field = ru.typeOf[HashMap[_,_]].decl(ru.TermName("cachedJavaKeySetHashCode")).asTerm
    (mirror, field)
  }
  def cachedJavaKeySetHashCode(map:  HashMap[Any, Any]) = {
    mirror.reflect(map).reflectField(field).get
  }

  private def value(i: Int) = new String("" + i)

  @Test def testCachedSizeAndHashCode(): Unit = {
    var map: HashMap[Any, Any] = emptyMap + mkTuple(O1, "O1_V1")
    assertEquals(1, map.size)
    map = map + mkTuple(O1, "O1_V2")
    val expected: HashMap[Any, Any] = emptyMap + mkTuple(O1, "O1_V2")
    assertSameEqHash(expected, map)
  }

  @Test def testCachedSizeAndHashCodeCollision(): Unit = {
    var map: HashMap[Any, Any] = emptyMap
    for (c <- cs)
      map = map.updated(c, value(c.i))
    var map1 = map
    for (c <- cs) {
      map1 = map1.updated(c, value(c.i))
      assertEquals(cachedJavaKeySetHashCode(map), cachedJavaKeySetHashCode(map1))
      if (c.i % 41 == 0)
        assertEquals(map, map1)
    }
    assertEquals(map, map1)
    assertSameEqHash(map1, map)

    var map2 = map + mkTuple(O1, "O1_V2")
    map2 = map2 +  mkTuple(O1, "O1_V2")
    assertSameEqHash(map1 + mkTuple(O1, "O1_V2"), map2)
  }

  @Test def replacedValue(): Unit = {
    val s1, s2 = new String("s") // equals, but distinct references,
    val key = "k"
    var map = emptyMap[Any, Any].updated(key, s1).updated(key, s2)
    Assert.assertSame(s2, map.apply(key))
    class collision() { override def hashCode = key.hashCode}
    for (i <- (0 to 1024)) map = map.updated(new collision(), "")
    Assert.assertSame(s1, map.updated(key, s1).apply(key))
  }

  @Test def replacedValueIdentical(): Unit = {
    case class A(a: Int)
    val map = emptyMap[Any, Any]
    val a = A(1)
    val map1 = map.updated(1, a)
    val map2 = map1.updated(1, a)
    assertSame(map1, map2)
  }

  @Test def replacedValueIdenticalCollision(): Unit = {
    val k0 = new C(0)
    val k1 = new C(4)
    assertEquals(k0.hashCode, k1.hashCode)
    val map = emptyMap[Any, Any].updated(k0, 0)
    val v1 = "v1"
    val map1 = map.updated(k1, v1)
    val map2 = map1.updated(k1, v1)
    assertSame(map1, map2)
  }

  @Test def nullValue(): Unit = {
    val map = emptyMap[Any, Any]
    assertEquals(Some(null), map.updated("", null).get(""))
  }

  @Test def nullValueCollision(): Unit = {
    val k0 = new C(0)
    val k1 = new C(4)
    assertEquals(k0.hashCode, k1.hashCode)
    val map = emptyMap[Any, Any].updated(k0, 0).updated(k1, null)
    assertEquals(Some(null), map.get(k1))
  }

  @Test def hashCodeCheck(): Unit = {
    assertEquals(359703249, collection.immutable.HashMap(1 -> 2).hashCode())
  }
}
