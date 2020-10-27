package scala

import org.junit.Assert.{assertArrayEquals, assertFalse, assertTrue}
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

  @Test
  def testArrayIsEmpty(): Unit = {
    def ie(a: Array[Int]) = a.isEmpty
    def ce(a: Array[Char]) = a.isEmpty // scala/bug#12172
    def re(a: Array[String]) = a.isEmpty
    def ge[T](a: Array[T]) = a.isEmpty
    val i0 = Array[Int]()
    val i1 = Array(1)
    val c0 = Array[Char]()
    val c1 = Array[Char](1)
    val s0 = Array[String]()
    val s1 = Array("")
    assertTrue(ie(i0) && ce(c0) && re(s0) && ge(i0) && ge(s0))
    assertFalse(ie(i1) || ce(c1) || re(s1) || ge(i1) || ge(s1))
  }
}
