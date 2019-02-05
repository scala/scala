package scala

import org.junit.Assert.assertArrayEquals
import org.junit.Test

import scala.runtime.BoxedUnit

class ArrayTest {
  @Test
  def testArrayCopyOfUnit(): Unit = {
    val expected = new Array[BoxedUnit](32).asInstanceOf[Array[AnyRef]]; java.util.Arrays.fill(expected, ().asInstanceOf[AnyRef])
    assertArrayEquals(expected, Array.copyOf(Array[Unit](), 32).asInstanceOf[Array[AnyRef]])
    assertArrayEquals(expected, Array.copyAs[Unit](Array[Nothing](), 32).asInstanceOf[Array[AnyRef]])
    assertArrayEquals(expected, Array.copyAs[Unit](Array[Unit](), 32).asInstanceOf[Array[AnyRef]])
  }
}
