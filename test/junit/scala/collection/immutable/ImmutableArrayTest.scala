package scala.collection.immutable

import org.junit.Assert
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class ImmutableArrayTest {
  @Test
  def slice(): Unit = {

    implicit def array2ImmutableArray[T](array: Array[T]): ImmutableArray[T] =
      ImmutableArray.unsafeWrapArray(array)

    val booleanArray = Array(true, false, true, false)
    check(booleanArray, Array(true, false), Array(false, true))

    val shortArray = Array(1.toShort, 2.toShort, 3.toShort, 4.toShort)
    check(shortArray, Array(1.toShort, 2.toShort), Array(2.toShort, 3.toShort))

    val intArray = Array(1, 2, 3, 4)
    check(intArray, Array(1, 2), Array(2, 3))

    val longArray = Array(1L, 2L, 3L, 4L)
    check(longArray, Array(1L, 2L), Array(2L, 3L))

    val byteArray = Array(1.toByte, 2.toByte, 3.toByte, 4.toByte)
    check(byteArray, Array(1.toByte, 2.toByte), Array(2.toByte, 3.toByte))

    val charArray = Array('1', '2', '3', '4')
    check(charArray, Array('1', '2'), Array('2', '3'))

    val doubleArray = Array(1.0, 2.0, 3.0, 4.0)
    check(doubleArray, Array(1.0, 2.0), Array(2.0, 3.0))

    val floatArray = Array(1.0f, 2.0f, 3.0f, 4.0f)
    check(floatArray, Array(1.0f, 2.0f), Array(2.0f, 3.0f))

    val refArray = Array("1", "2", "3", "4")
    check[String](refArray, Array("1", "2"), Array("2", "3"))

    def unit1(): Unit = {}
    def unit2(): Unit = {}
    Assert.assertEquals(unit1, unit2)
    // unitArray is actually an instance of Immutable[BoxedUnit], the check to which is actually checked slice
    // implementation of ofRef
    val unitArray: ImmutableArray[Unit] = Array(unit1, unit2, unit1, unit2)
    check(unitArray, Array(unit1, unit1), Array(unit1, unit1))
  }

  private def check[T](array: ImmutableArray[T], expectedSliceResult1: ImmutableArray[T], expectedSliceResult2: ImmutableArray[T]) {
    Assert.assertEquals(array, array.slice(-1, 4))
    Assert.assertEquals(array, array.slice(0, 5))
    Assert.assertEquals(array, array.slice(-1, 5))
    Assert.assertEquals(expectedSliceResult1, array.slice(0, 2))
    Assert.assertEquals(expectedSliceResult2, array.slice(1, 3))
    Assert.assertEquals(ImmutableArray.empty[Nothing], array.slice(1, 1))
    Assert.assertEquals(ImmutableArray.empty[Nothing], array.slice(2, 1))
  }
}