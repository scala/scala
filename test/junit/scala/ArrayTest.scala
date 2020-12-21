package scala

import org.junit.Assert.{ assertArrayEquals, assertFalse, assertTrue }
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
    assertTrue(Array[Int]().isEmpty)
    assertTrue(Array[Char]().isEmpty) // scala/bug#12172
    assertTrue(Array[String]().isEmpty)

    assertFalse(Array(1).isEmpty)
    assertFalse(Array[Char](1).isEmpty)
    assertFalse(Array("").isEmpty)

    def ge[T](a: Array[T]) = a.isEmpty

    assertTrue(ge(Array[Int]()))
    assertTrue(ge(Array[Char]()))
    assertTrue(ge(Array[String]()))

    assertFalse(ge(Array(1)))
    assertFalse(ge(Array[Char]('x')))
    assertFalse(ge(Array("")))
  }

  /** Test Array.equals (sameElements) for reference arrays. */
  @Test def testArraySemiDeepEquality(): Unit = {
    val xs = Array(List(1, 2, 3))
    val ys = Array(Vector(1, 2, 3))
    def anyxs = xs.asInstanceOf[Array[AnyRef]]
    def anyys = ys.asInstanceOf[Array[AnyRef]]
    assertTrue("Arrays of List and Vector should compare equal", Array.equals(anyxs, anyys))
    assertTrue(xs.sameElements(ys))     // for fun
    //assertTrue(Array.equals(xs, ys))  // would be nice
    assertTrue("Arrays of String", Array.equals(Array[AnyRef]("hello, world"), Array[AnyRef]("hello, world")))
    assertFalse("Arrays of String", Array.equals(Array[AnyRef]("hello, world"), Array[AnyRef]("goodbye, cruel world")))
  }
}
