package scala.collection
package mutable

import org.junit.Assert._
import org.junit.Test

import scala.tools.testkit.ReflectUtil.getMethodAccessible

class LongMapTest {

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

}
