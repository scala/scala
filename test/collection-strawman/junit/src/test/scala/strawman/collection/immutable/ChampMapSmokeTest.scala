package strawman.collection.immutable

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test

object ChampMapSmokeTest {

  private def emptyMap[K, V]: ChampHashMap[K, V] =
    ChampHashMap.empty[K, V]

  private def mapOf[K, V](keyValuePairs: (K, V)*): ChampHashMap[K, V] = {
    val builder = ChampHashMap.newBuilder[K, V]()
    keyValuePairs.foreach(builder.addOne)
    builder.result
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
    val map: ChampHashMap[Int, Int] = emptyMap

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
    val map: ChampHashMap[Int, Int] = emptyMap
    val res1 = map + mkTuple(1) + mkTuple(2)
    val res2 = res1 + mkTuple(32769) - 2
    /* should trigger assertion in data structure if not compacting */
  }

  @Test def testCheckCompactionFromMiddleUponDelete(): Unit = {
    val map: ChampHashMap[Int, Int] = emptyMap
    val res1 = map + mkTuple(1) + mkTuple(2) + mkTuple(65) + mkTuple(66)
    val res2 = res1 + mkTuple(32769) - 66
    assert(!(res1 == res2))
  }

  @Test def testCheckCompactionFromBeginUponDelete_HashCollisionNode1(): Unit = {
    val map: ChampHashMap[CustomHashInt, CustomHashInt] = emptyMap

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
    val map: ChampHashMap[CustomHashInt, CustomHashInt] = emptyMap

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
    val expected = mapOf(mkTuple(v1h1), mkTuple(v32769a))
    assertEquals(expected, res3)
  }

  @Test def testCheckCompactionFromBeginUponDelete_HashCollisionNode3(): Unit = {
    val map: ChampHashMap[CustomHashInt, CustomHashInt] = emptyMap
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
    val map: ChampHashMap[CustomHashInt, CustomHashInt] = emptyMap
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
    val map: ChampHashMap[Int, Int] = emptyMap + mkTuple(63, 65)
    assertTrue(map.contains(63))
    assertEquals(65, map.get(63).get)
  }

  @Test def testRemoveFromSingleton(): Unit = {
    val map: ChampHashMap[Int, Int] = emptyMap + mkTuple(63, 65)
    val res = map - 63
    assertTrue(res.isEmpty)
    assertFalse(res.contains(63))
    assertEquals(emptyMap, res)
  }

}
