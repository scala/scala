package scala.collection.mutable

import org.junit.Test
import org.junit.Assert.assertEquals
import scala.tools.testkit.AssertUtil.assertThrows

import scala.runtime.PStatics
class ArrayBuilderTest {

  /* Test for scala/bug#12617 */

  val MaxArraySize: Int = PStatics.VM_MaxArraySize

  @Test
  def testCapacityGrowthLimit: Unit = {

    val ab: ArrayBuilder[Unit] = ArrayBuilder.make[Unit]

    var i: Int = 0
    while (i < MaxArraySize) {
      ab.addOne(())
      i += 1
    }

    // reached maximum size without entering an infinite loop?
    assertEquals(ab.length, MaxArraySize)

    // expect an exception when trying to grow larger than maximum size by addOne
    assertThrows[Exception](ab.addOne(()))

    val arr:Array[Unit] = Array[Unit]((), (), (), (), (), (), (), (), (), (), (), ())

    // expect an exception when trying to grow larger than maximum size by addAll(iterator)
    assertThrows[Exception](ab.addAll(arr.iterator))

    // expect an exception when trying to grow larger than maximum size by addAll(array)
    assertThrows[Exception](ab.addAll(arr))

  }
}
