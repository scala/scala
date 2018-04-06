package strawman.collection.immutable

import strawman.collection.Hashing
import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class HashMapTest {

  private val computeHashF = {
    Hashing.computeHash _
  }

  @Test
  def canMergeIdenticalHashMap1sWithNullKvs(): Unit = {
    def m = new HashMap.HashMap1(1, computeHashF(1), 1, null)
    val merged = m.merged(m)(null)
    assertEquals(m, merged)
  }

  @Test
  def canMergeIdenticalHashMap1sWithNullKvsCustomMerge(): Unit = {
    def m = new HashMap.HashMap1(1, computeHashF(1), 1, null)
    val merged = m.merged(m) {
      case ((k1, v1), (k2, v2)) =>
        (k1, v1 + v2)
    }
    assertEquals(new HashMap.HashMap1(1, computeHashF(1), 2, null), merged)
  }

  @Test
  def canMergeHashMap1sWithNullKvsHashCollision(): Unit = {
    val key1 = 1000L * 1000 * 1000 * 10
    val key2 = key1.##.toLong
    assert(key1.## == key2.##)

    val m1 = new HashMap.HashMap1(key1, computeHashF(key1.##), 1, null)
    val m2 = new HashMap.HashMap1(key2, computeHashF(key2.##), 1, null)
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
}