package scala.collection
package mutable

import org.junit.Assert._
import org.junit.Test

import scala.tools.testkit.ReflectUtil.getMethodAccessible

class LongMapTest {
  @Test def t13048(): Unit = {
    def t(x: Int, y: Int): Unit = {
      val m = LongMap.empty[Unit]
      m.getOrElseUpdate(x, m.getOrElseUpdate(y, ()))
      assertEquals(immutable.Set(x, y), m.keys.toSet)
    }
    t(4, 28)
    t(28, 4)
    t(4, 4)
  }

  @Test
  def `repack calculation must complete`: Unit = {
    val vacant: Int = 10256777
    val mask: Int   = 1073741823
    val size: Int   = 603979777
    //LongMap.repackMask
    val name = "scala$collection$mutable$LongMap$$repackMask"
    val sut = getMethodAccessible[LongMap.type](name)
    val res = sut.invoke(LongMap, mask, size, vacant)
    assertEquals(1073741823, res)
  }

  @Test
  def `put of MinValue`: Unit = {

    // LongMap.put should return Some for existing keys
    val m = LongMap.empty[Int]
    assertEquals(None, m.put(0L, 1))
    assertEquals("put on existing key 0 should return Some(1)", Some(1), m.put(0L, 2))

    assertEquals(None, m.put(1L, 10))
    assertEquals("put on existing key 1 should return Some(10)", Some(10), m.put(1L, 20))

    // Regression test: LongMap.put returning None for existing Long.MinValue key
    assertEquals(None, m.put(Long.MinValue, 100))
    assertEquals("put on existing Long.MinValue key should return Some(100)", Some(100), m.put(Long.MinValue, 200))

    assertEquals(None, m.put(Long.MaxValue, 300))
    assertEquals(Some(300), m.put(Long.MaxValue, 400))

    assertEquals(None, m.put(999L, 1))
  }
}
